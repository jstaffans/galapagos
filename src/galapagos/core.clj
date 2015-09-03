(ns galapagos.core
  (:require [galapagos.query :refer [parse]]
            [galapagos.schema :as schema]
            [clojure.core.async :as async]
            [muse.core :as muse])
  (:refer-clojure :exclude [compile]))

(defprotocol Solvable
  (solve [this value])
  (arity [this])
  (acc-fn [this]))

(defprotocol Visited
  (done? [this]))

;; Wraps the solution as a muse DataSource
(defrecord SolvableNode [node query fields]
  Solvable
  (solve [_ value]
    ;; if we don't have arguments, solve using the parent value
    (let [args (merge (:args query) value)]
      (reify
        muse/DataSource
        (fetch [_]
          ((:solve node) args))

        muse/LabeledSource
        (resource-id [_] args))))

  (arity [_] (:arity node))

  ;; At solvable nodes, we introduce a new level of nesting
  (acc-fn [_]
    #(assoc {} (or (:alias query) (:name query)) %))

  Visited
  (done? [_] false))

(defrecord SolvableField [fields key-names]
  Solvable
  (solve [_ value]
    (let [solution (reduce #(assoc %1 %2 (get value %2)) {} key-names)]
      (reify
        muse/DataSource
        (fetch [_] (async/go solution))

        muse/LabeledSource
        (resource-id [_] value))))

  (arity [_] :one)

  ;; At leaves, we use the result directly
  (acc-fn [_] #(into {} %))

  Visited
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
                        (let [field (get-in node [:fields (:name query) :type])]
                          (compile root field query)))
                  (:fields query))]
     ;; Don't return the query root of the schema, instead return
     ;; the first subtree (the actual query) which is easier to traverse.
     ;; Multiple top-level query trees are not currently supported.
     (if (= root node)
       (do
         (assert (= 1 (count fields)) "Multiple, sibling solvable nodes are not supported at the root level!")
         (first fields))

       (if (schema/primitive? node)
         (->SolvableField [node] [(:name query)])
         (->SolvableNode node query (merge-children fields)))))))

;; deprecated for execution - might be a good approach for introspection though
(defn- walk
  "Walks the graph, providing solutions as inputs to child elements.
   The result is a solved context. This is a naive approach and not
   as efficient as traversal with muse (see the traverse function)."
  ([context] (walk context {}))
  ([context parent-solution]
   (mapv
     (fn [query field]
       (let [solution (solve field parent-solution)]
         {(:name query) {:solution solution
                         :fields   (when (not (nil? solution))
                                     (if (= (arity field) :many)
                                       (mapv (partial walk field) solution)
                                       (walk field solution)))}}))
     (get-in context [:query :fields])
     (get-in context [:fields]))))



(declare traverse)
(defn- empty-node?
  "Check if a node is empty (either lacks a solution or doesn't have any children).
  We can stop the recursion in this case and return an empty result."
  [field solution]
  (or (empty? solution) (empty? (:fields field))))


(defn traverse-one
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

(defn traverse-many
  [graph field parent]
  ;; Solution is a collection of traversable nodes.
  ;; Use muse/traverse to iterate over all of them.
  (muse/traverse
    (fn [solution]
      (if (empty-node? field solution)
          (muse/value {})
          (traverse field solution)))
    (solve graph parent)))

(defn traverse
  ([graph] (traverse graph {}))
  ([graph parent]
   (let [fields (:fields graph)
         traverse-fn (if (= (arity graph) :many) traverse-many traverse-one)]

     (muse/fmap
       (acc-fn graph)
       (apply (partial muse/fmap
                (fn [& muses]
                  ; merge the sibling muses into one list
                  (if (= (arity graph) :one)
                    (into {} muses)
                    (apply (partial map merge) muses))))
         (map #(traverse-fn graph % parent) fields))))))



;; Example graphs that can be traversed with muse
(comment

  ;; (muse/run!! (traverse simple-graph))
  (def simple-graph
    (galapagos.core/map->SolvableNode
      {:node   galapagos.example.schema/FindPost
       :query  {:name :post :args {:id 1}}

       :fields [(galapagos.core/map->SolvableField
                  {:fields    [galapagos.schema/GraphQLString]
                   :key-names [:title]})]})))


(defn execute!
  "Non-blocking execution - returns a core.async channel with the result.
  See execute!! for the blocking version.

  (galapagos.core/execute!
    galapagos.schema/create-schema galapagos.example.schema/QueryRoot)
    \"{ post(id: 1) { title } }\"))"
  [{:keys [root]} query-string]
  (muse/run!
    (let [query (parse query-string)
          graph (compile root query)]
      (traverse graph))))

(defn execute!!
  "Blocking execution. See execute! for the non-blocking version.

  (galapagos.core/execute!!
    galapagos.schema/create-schema galapagos.example.schema/QueryRoot)
   \"{ post(id: 1) { title } }\"))"
  [& args]
  (async/<!! (apply execute! args)))

