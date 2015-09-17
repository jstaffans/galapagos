(ns galapagos.core
  (:require [galapagos.query :refer [parse]]
            [galapagos.util :as util]
            [clojure.core.async :as async]
            [schema.coerce :as coerce]
            [muse.core :as muse]
            [galapagos.schema :as schema])
  (:refer-clojure :exclude [compile]))

;; ## Core
;;
;; Contains the main galapagos entry point and provides an execution
;; engine based on the [muse library](https://github.com/kachayev/muse).
;; A graph is formed based on the query and the GraphQL schema, in which
;; the leaves return muse `DataSource`s, which in turn will wrap core.async
;; channels. Data will be fetched from the leaves in a concurrent fashion,
;; with caching to avoid fetching the same data more than once.

;; Something that will return a solution as a muse `DataSource`.
(defprotocol Solvable
  (solve [this value]))

;; Provides information on how be used to collect the data returned by the
;; node itself and its children into the final result graph.
(defprotocol ResultAccumulator
  (acc-fn [this]))

;; The `Visited` protocol is helpful for knowing how to traverse a node's
;; children and when to terminate the traversal. A node that returns
;; many elements will have an arity of `:many`, a single element an arity
;; of `:one`. A special case is the root node which isn't directly
;; solvable.
(defprotocol Visited
  (arity [this])
  (empty-node? [this])
  (done? [this]))

;; The root node of the query. This node does not have any fields
;; of its own, therefore it is not solvable. Only its children are.
(defrecord SolvableRoot [node fields]
  ResultAccumulator
  (acc-fn [_] #(assoc {} :data %))

  Visited
  (arity [_] :root)
  (empty-node? [_] false)
  (done? [_] false))

(defn- resolve-name
  "Takes any aliases into account when resolving the name of a field."
  [query]
  (or (:alias query) (:name query)))

(defn- coerced-args
  "Coerces field input arguments using a basic string coercion matcher."
  [node query]
  (let [coercer (coerce/coercer (:args node) coerce/string-coercion-matcher)
        args (or (:args query) {})
        coerced (coercer args)]
    (if-let [error-val (schema.utils/error-val coerced)]
      (throw (IllegalArgumentException. (str "Input argument coercion failed at " (:name query) " (" error-val ")")))
      coerced)))

;; A GraphQL "object" node that resolves to other objects (more nodes)
;; or fields (leaves).
(defrecord SolvableNode [node query fields]
  Solvable
  (solve [_ value]
    ;; if we don't have arguments, solve using the parent value
    (let [args (merge (coerced-args node query) value)]
      (reify
        muse/DataSource
        (fetch [_]
          ;; modify the solution with a transducer that maps the solution
          ;; to the type of the node. Easier to deal with in child nodes.
          (let [out-chan (async/chan 1
                           (map
                             (fn [solution]
                               (util/apply-1 #(assoc {} (:type node) %) solution))))]
            (async/pipe ((:solve node) args) out-chan)))

        muse/LabeledSource
        (resource-id [_] [args (resolve-name query)]))))


  ResultAccumulator
  ;; At solvable nodes, we introduce a new level of nesting
  (acc-fn [_]
    #(assoc {} (resolve-name query) %))

  Visited
  (arity [_] (:arity node))
  (empty-node? [_] (empty? fields))
  (done? [_] false))


;; A leaf node that looks up keys in a parent map or record.
(defrecord SolvableLookupField [fields key-names]
  Solvable
  (solve [_ parent]
    (let [value (first (vals parent))
          solution (reduce (fn [acc k]
                             ;; if keys have "on" metadata, it comes from a fragment and
                             ;; we have to check that the value is of the correct type
                             (let [key-meta (:on (meta k))
                                   value-meta (:type (meta value))]
                               (if (and key-meta value-meta (not= key-meta value-meta))
                                 acc
                                 (assoc acc (:name k) (get value (:name k))))))
                     {} key-names)]
      (reify
        muse/DataSource
        (fetch [_] (async/go solution))

        muse/LabeledSource
        (resource-id [_] value))))

  ResultAccumulator
  ;; At leaves, we use the result directly
  (acc-fn [_] #(into {} %))

  Visited
  (arity [_] :one)
  (empty-node? [_] (empty? fields))
  (done? [_] true))


;; A leaf node that resolves directly to a value.
(defrecord SolvableRawField [node query fields]
  Solvable
  (solve [_ value]
    (let [args (merge (coerced-args node query) value)]
      (reify
        muse/DataSource
        (fetch [_]
          ;; Use a transducer to associate the raw solution to the name of the field
          (let [out-chan (async/chan 1 (map #(assoc {} (resolve-name query) %)))]
            (async/pipe ((:solve node) args) out-chan)))

        muse/LabeledSource
        (resource-id [_] [value (resolve-name query)]))))

  ResultAccumulator
  (acc-fn [_] #(into {} %))

  Visited
  (arity [_] :one)
  (empty-node? [_] false)
  (done? [_] true))

(defn- merge-children
  "Merges SolvableLookupField children to a single SolvableLookupField with multiple
  fields and key-names. Keeps traversal lean, because we don't have to visit each leaf
  separately.

  Example:

    [#SolvableLookupField{:fields [{:type GraphQLInt}],
                          :key-names [{:name :id}]}
     #SolvableLookupField{:fields [GraphQLString],
                          :key-names [{:name :fullname]}]

    -> [#SolvableLookupField{:fields [{:type GraphQLInt}
                                      {:type GraphQLString}],
                             :key-names [{:name :id}
                                         {:name :fullname}]}]
   "
  [fields]
  (let [mergeable? #(instance? SolvableLookupField %)
        mergeable-fields (filter mergeable? fields)
        other-fields (into [] (filter (complement mergeable?) fields))]
    (->> mergeable-fields
         ((juxt
            #(apply concat (mapv :fields %))
            #(apply concat (mapv :key-names %))))
         (apply galapagos.core/->SolvableLookupField)
         (conj other-fields))))

(defn- get-field-definition
  "Gets the definition of a field. The source of this information may be in the node or,
  if the field is part of an interface, in the map of interface definitions at the root
  of the schema graph."
  [root node query]
  (if-let [field (or
                   (get-in (:type node) [:fields (:name query)])
                   (first (keep
                            (fn [interface]
                              (get-in root [:interfaces interface :fields (:name query)]))
                            (-> (:type node) :type-definition :interfaces))))]
    field
    (throw (IllegalStateException. (str "Could not find definition for field " (:name query))))))

(defn- inline-fragment?
  "Checks if a fragment is defined in-line or separately. If it's defined
  separately, there will be a keyword reference to a fragment name."
  [fragment]
  (not (keyword? fragment)))

(defn- attach-type-metadata
  "For fragments, it's important to know which types they apply to.
  This function attaches this information as a piece of metadata.0"
  [on field]
  (with-meta field {:on on}))

(defn- collect-fields
  "Collects fields defined in fragments and merges then with the selection fields."
  [query fragments]
  (reduce (fn [acc fragment]
            (let [f (if (inline-fragment? fragment) fragment (get-in fragments [fragment]))]
              (into acc (mapv (partial attach-type-metadata (:on f)) (:fields f)))))
    (:fields query) (:fragments query)))

(defn- returns-scalar?
  "Checks if a particular node is actually a leaf that returns a scalar.
  This is the case for leaves that represent a calculation of some kind
  as opposed to a simple map lookup."
  [node]
  (let [ret (:returns (:type node))]
    (and ret (not (vector? ret)) (schema/scalar? node))))

(defn- as-node
  "In this implementation, a node is always represented by a map with a `:type` key.
  We don't define for example the root node this way, however, so we need to coerce
  it into a form that the compiler understands."
  [field]
  (assoc {} :type field))

(defn- compile
  "Compiles query into a traversable graph."
  ([root query] (compile root (as-node root) (:fragments query) (dissoc query :fragments)))
  ([root node fragments query]
   (let [fields (mapv (fn [query]
                        (let [field (get-field-definition root node query)]
                          (compile root field fragments query)))
                  (collect-fields query fragments))
         type (:type node)]
     (if (= root type)
       (->SolvableRoot node fields)

       (cond
         (returns-scalar? node) (->SolvableRawField type query [type])
         (schema/scalar? node) (->SolvableLookupField [type] [query])
         :else (->SolvableNode type query (merge-children fields)))))))

(declare traverse)

(defmulti traverse-node (fn [graph _ _] (arity graph)))

;; No solving at the root node
(defmethod traverse-node :root
  [_ field parent]
  (traverse field parent))

;; Solution is either a leaf node (which we'll solve directly,
;; terminating the recursion) or a single new traversable node.
(defmethod traverse-node :one
  [graph field parent]
  (if (done? graph)
    (solve graph parent)

    (muse/flat-map
      (fn [solution]
        (if (or (empty? solution) (empty-node? field))
          (muse/value {})
          (traverse field solution)))
      (solve graph parent))))

;; Solution is a collection of traversable nodes.
;; Use `muse/traverse` to iterate over all of them.
(defmethod traverse-node :many
  [graph field parent]
  (muse/traverse
    (fn [solution]
      (if (or (empty? solution) (empty-node? field))
        (muse/value {})
        (traverse field solution)))
    (solve graph parent)))

(defmulti merge-siblings (fn [graph _] (arity graph)))

(defmethod merge-siblings :default [_ muses] (into {} muses))

(defmethod merge-siblings :many [_ muses] (apply (partial map merge) muses))

(defn- traverse
  "Takes the traversable graph produce by `galapagos.core/compile` and
  traverses it using the muse library. If everything goes well (ie all
  leaves correctly solve to muse DataSources, siblings are merged
  correctly and node arity is taken into account), the result will
  be a normal map with the result, ready to be returned to the caller."
  ([graph] (traverse graph {}))
  ([graph parent]
   (muse/fmap
     (acc-fn graph)
     (apply (partial muse/fmap
              (fn [& muses]
                ; merge the sibling muses into one map or list of maps
                (merge-siblings graph muses)))
       (map #(traverse-node graph % parent) (:fields graph))))))


(defn execute!
  "Non-blocking execution - returns a core.async channel with the result.
  See execute!! for the blocking version.

    (galapagos.core/execute!
      (galapagos.schema/create-schema
        galapagos.example.schema/QueryRoot)
      \"{ post(id: 1) { title } }\"))"
  [{:keys [root]} query-string]
  (let [query (parse query-string)
        graph (compile root query)]
    (muse/run! (traverse graph))))

(defn execute!!
  "Blocking execution. See execute! for the non-blocking version.

    (galapagos.core/execute!!
      (galapagos.schema/create-schema
        galapagos.example.schema/QueryRoot)
      \"{ post(id: 1) { title } }\"))"
  [& args]
  (async/<!! (apply execute! args)))

