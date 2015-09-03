(ns galapagos.core
  (:require [galapagos.query :refer [parse]]
            [galapagos.schema :as schema]
            [clojure.core.async :as async]
            [muse.core :as muse])
  (:refer-clojure :exclude [compile]))

(declare traverse traverse-root traverse-one traverse-many)

(defprotocol Solvable
  (solve [this value])
  (arity [this])
  (traverse-fn [this])
  (acc-fn [this]))

(defprotocol Visited
  (solves-to-primitive? [this])
  (done? [this]))

(defrecord SolvableRoot [node fields]
  Solvable
  (solve [_ _] (throw (IllegalStateException. "Root node should not be directly solved!")))
  (arity [_] :one)
  (traverse-fn [_] traverse-root)
  (acc-fn [_] #(assoc {} :data %))

  Visited
  (solves-to-primitive? [_] false)
  (done? [_] false))


(defrecord SolvableNode [node query fields]
  Solvable
  (solve [this value]
    ;; if we don't have arguments, solve using the parent value
    (let [args (merge (:args query) value)]
      (reify
        muse/DataSource
        (fetch [_]
          ;; modify the solution with a transducer that maps the solution
          ;; to the type of the node. Easier to deal with in child nodes.
          (let [result-chan (async/chan 1
                              (map (fn [solution]
                                     ;; If we're getting a primitive, there won't be any child nodes.
                                     ;; Just return the raw solution.
                                     (if (solves-to-primitive? this)
                                       solution
                                       (if (vector? solution)
                                         (into [] (map #(assoc {} (:type node) %) solution))
                                         (assoc {} (:type node) solution))))))]
            (async/pipe ((:solve node) args) result-chan)))

        muse/LabeledSource
        (resource-id [_] args))))

  (arity [_] (:arity node))

  (traverse-fn [this]
    (if (= (arity this) :many) traverse-many traverse-one))

  ;; At solvable nodes, we introduce a new level of nesting
  (acc-fn [_]
    #(assoc {} (or (:alias query) (:name query)) %))

  Visited
  ;; Nodes may solve to a primitive
  (solves-to-primitive? [_] (schema/primitive? (:returns node)))
  (done? [this] (solves-to-primitive? this)))

(defrecord SolvableField [fields key-names]
  Solvable
  (solve [_ value]
    ;; value is the parent map from which we are fetching keys.
    ;; It'll be in the form of {:parent { .. fields .. }}
    (let [solution (reduce #(assoc %1 %2 (get (first (vals value)) %2)) {} key-names)]
      (reify
        muse/DataSource
        (fetch [_] (async/go solution))

        muse/LabeledSource
        (resource-id [_] value))))

  (arity [_] :one)

  (traverse-fn [_] traverse-one)

  ;; At leaves, we use the result directly
  (acc-fn [_] #(into {} %))

  Visited
  ;; Fields are records or maps, not primitives
  (solves-to-primitive? [_] false)
  (done? [_] true))


(defn- merge-children
  "Merges SolvableField children to a single SolvableField with multiple
   fields and key-names. Keeps traversal lean, because we don't have to
   visit each leaf separately.

   Example:

   [#SolvableField{:fields [GraphQLInt], :key-names [:id]}
    #SolvableField{:fields [GraphQLString], :key-names [:name]}]

    -> [#SolvableField{:fields [GraphQLInt GraphQLString], :key-names [:id :name]}]
   "
  [fields]
  (let [mergeable? #(instance? SolvableField %)
        mergeable-fields (filter mergeable? fields)
        other-fields (into [] (filter (complement mergeable?) fields))]
    (->> mergeable-fields
         ((juxt
            #(apply concat (mapv :fields %))
            #(apply concat (mapv :key-names %))))
         (apply galapagos.core/->SolvableField)
         (conj other-fields))))

(defn- compile
  "Compiles query into a traversable graph."
  ([root query] (compile root root query))
  ([root node query]
   (let [fields (mapv (fn [query]
                        (let [field (or
                                      ;; types can be defined either at individual nodes
                                      ;; or at the root of the schema
                                      (get-in node [:fields (:name query) :type])
                                      (get-in root [:fields (:name query) :type]))]
                          (compile root field query)))
                  (:fields query))]
     ;; Don't return the query root of the schema, instead return
     ;; the first subtree (the actual query) which is easier to traverse.
     ;; Multiple top-level query trees are not currently supported.
     (if (= root node)
       (->SolvableRoot node fields)

       (if (schema/primitive? node)
         (->SolvableField [node] [(:name query)])
         (->SolvableNode node query (merge-children fields)))))))


(defn- empty-node?
  "Check if a node is empty (either lacks a solution or doesn't have any children).
  We can stop the recursion in this case and return an empty result."
  [field solution]
  (or (empty? solution) (empty? (:fields field))))

(defn- traverse-root
  [_ field parent]
  (traverse field parent))

(defn- traverse-one
  [graph field parent]
  ;; Solution is either a leaf node (which we'll solve directly,
  ;; terminating the recursion) or a single new traversable node.
  (if (done? graph)
    (solve graph parent)

    (muse/flat-map
      (fn [solution]
        (if (empty-node? field solution)
          (muse/value {})
          (traverse field solution)))
      (solve graph parent))))

(defn- traverse-many
  [graph field parent]
  ;; Solution is a collection of traversable nodes.
  ;; Use muse/traverse to iterate over all of them.
  (muse/traverse
    (fn [solution]
      (if (empty-node? field solution)
          (muse/value {})
          (traverse field solution)))
    (solve graph parent)))

(defmulti merge-siblings (fn [graph _] (arity graph)))

(defmethod merge-siblings :one [graph muses] (if (solves-to-primitive? graph)
                                               (first muses)
                                               (into {} muses)))

(defmethod merge-siblings :many [_ muses] (apply (partial map merge) muses))

(defn- traverse
  ([graph] (traverse graph {}))
  ([graph parent]
   (muse/fmap
     (acc-fn graph)
     (apply (partial muse/fmap
              (fn [& muses]
                ; merge the sibling muses into one list
                (merge-siblings graph muses)))
       (map #((traverse-fn graph) graph % parent) (:fields graph))))))


(defn execute!
  "Non-blocking execution - returns a core.async channel with the result.
  See execute!! for the blocking version.

  (galapagos.core/execute!
    galapagos.schema/create-schema galapagos.example.schema/QueryRoot)
    \"{ post(id: 1) { title } }\"))"
  [{:keys [root]} query-string]
  (let [query (parse query-string)
        graph (compile root query)]
    (muse/run! (traverse graph))))

(defn execute!!
  "Blocking execution. See execute! for the non-blocking version.

  (galapagos.core/execute!!
    galapagos.schema/create-schema galapagos.example.schema/QueryRoot)
   \"{ post(id: 1) { title } }\"))"
  [& args]
  (async/<!! (apply execute! args)))

