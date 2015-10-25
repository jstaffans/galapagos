(ns galapagos.core
  (:require [galapagos.query :refer [parse]]
            [galapagos.util :as util]
            [clojure.core.async :as async]
            [schema.coerce :as coerce]
            [muse.core :as muse]
            [taoensso.timbre :as log]
            [galapagos.schema :as schema]
            [galapagos.introspection :as introspection])
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
  (let [coercer (coerce/coercer (into {} (map :coercion (vals (:args node)))) coerce/string-coercion-matcher)
        args (or (:args query) {})
        coerced (coercer args)]
    (if-let [error-val (schema.utils/error-val coerced)]
      (do
        (log/error error-val)
        (throw (IllegalArgumentException. (str "Could not coerce input argument at " (:name query) ":"))))
      coerced)))

(defn- assoc-it
  "Associate a solution with a well-known key for easier access in child nodes."
  [solution]
  (util/apply-1 #(assoc {} schema/it-key %) solution))

(defn- empty-result?
  "Check if v, if a coll, is empty. Returns false if v is not a coll."
  [result]
  (or
    (nil? result)
    (and (map? result) (coll? (vals result)) (every? empty? (vals result)))
    (and (coll? result) (empty? result))))

(defn- filter-not-required
  "Set non-required fields to be empty in the parent object."
  [node result]
  {:pre (map? result)}
  (reduce-kv
    (fn [acc k v]
      (let [required? (get-in node [:fields k :required?])]
        (if (and (false? required?) (empty-result? v)) acc (assoc acc k v))))
    {}
    result))

;; A GraphQL "object" node that resolves to other objects (more nodes)
;; or fields (leaves).
(defrecord SolvableNode [node query fields]
  Solvable
  (solve [_ value]
    ;; if we don't have arguments, solve using the parent value
    (let [args (merge (coerced-args node query) value)
          node-chan (async/chan 1 (map assoc-it))]
      (reify
        muse/DataSource
        (fetch [_]
          ;; modify the solution with a transducer that maps the solution
          ;; to a well-known key (:__OBJ) which makes access in child nodes easier.
          (async/pipe ((:solve node) args) node-chan))

        muse/LabeledSource
        (resource-id [_] [args (resolve-name query)]))))


  ResultAccumulator
  ;; At solvable nodes, we introduce a new level of nesting
  (acc-fn [_]
    #(let [result (assoc {} (resolve-name query) (util/apply-1 (partial filter-not-required node) %))]
      (if (empty? result) nil result)))

  Visited
  (arity [_] (:arity node))
  (empty-node? [_] (empty? fields))
  (done? [_] false))


(defn- key-applies?
  "If keys have \"on\" metadata, it comes from a fragment and
  we have to check that the value is of the correct type."
  [key parent]
  (let [key-meta (:on (meta key))
        value-meta (:type (meta parent))]
    (not (and key-meta value-meta (not= key-meta value-meta)))))

;; A leaf node that looks up keys in a parent map or record.
(defrecord SolvableLookupField [fields key-names]
  Solvable
  (solve [_ parent]
    (let [value (schema/it parent)
          solution
          (if (empty? value)
            {}
            (reduce (fn [acc k]
                      (if (key-applies? k value)
                        (assoc acc (:name k) (get value (:name k)))
                        acc))
              {} key-names))]
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



;; A leaf node that looks up keys in a parent map or record.
(defrecord SolvableRecursiveField [node query fields]
  Solvable
  (solve [_ parent]
    (let [value (schema/it parent)
          solution (if (key-applies? query value)
                     (or (get value (:name query)) {})
                     {})]
      (reify
        muse/DataSource
        (fetch [_] (async/go (assoc-it solution)))

        muse/LabeledSource
        (resource-id [_] parent))))

  ResultAccumulator
  (acc-fn [_]
    #(assoc {} (resolve-name query) %))

  Visited
  (arity [_] :one)
  (empty-node? [_] (empty? fields))
  (done? [_] false))

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

(defmulti field-definition-in-type
  "Determines the manner in which a field type is defined in a node. It may be a reference
  to a field var, or be defined directly in the node. If neither, it may be part of an interface,
  in which case it is not defined in the node itself."
  (fn [type field]
    (if-let [definition (get-in type [:fields field])]
      (if (contains? definition :var)
        :reference-to-field-var
        :direct)
      :not-defined-in-node)))

(defmethod field-definition-in-type :direct
  [type field]
  (get-in type [:fields field]))

(defmethod field-definition-in-type :reference-to-field-var
  [type field]
  (assoc {} :type (-> (get-in type [:fields field :var]) deref)))

(defmethod field-definition-in-type :not-defined-in-node
  [_ _] nil)

(defn- get-field-definition
  "Gets the definition of a field. The source of this information may be in the node or,
  if the field is part of an interface, in the map of interface definitions at the root
  of the schema graph."
  [root node query]
  (if-let [field (or
                   (field-definition-in-type (:type node) (:name query))

                   ;; This is mainly used by introspection types, which are added
                   ;; to the root during a pre-processing step. Make sure we find
                   ;; those as well by looking in the root of the query schema.
                   (get-in root [:fields (:name query)])

                   ;; Check interfaces for field definition.
                   (let [type-def (-> (:type node) :type-definition)
                         interfaces (:interfaces type-def)
                         possibles (map (comp find-var symbol) (:possibleTypes type-def))]
                     (first (keep
                              (fn [i-var]
                                (field-definition-in-type (deref i-var) (:name query)))
                              (concat interfaces possibles)))))]
    field
    (throw (IllegalStateException. (str "Could not find definition for field " (:name query))))))

(defn- inline-fragment?
  "Checks if a fragment is defined in-line or separately. If it's defined
  separately, there will be a keyword reference to a fragment name."
  [fragment]
  (not (keyword? fragment)))

(defn- attach-type-metadata
  "For fragments, it's important to know which types they apply to.
  This function attaches this information as a piece of metadata."
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
    (and ret (contains? #{:ENUM :SCALAR} (:kind ret)))))

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
         (returns-scalar? node)   (->SolvableRawField type query [type])
         (schema/scalar? node)    (->SolvableLookupField [type] [query])
         (schema/recursive? node) (->SolvableRecursiveField type query (merge-children fields))
         :else                    (->SolvableNode type query (merge-children fields)))))))

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
  "Takes the graph produce by `galapagos.core/compile` and traverses it using the muse library.
  If everything goes well (ie all leaves correctly solve to muse DataSources, siblings are merged
  correctly and node arity is taken into account), the result will be a normal map with the result,
  ready to be returned to the caller."
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

(defn create-schema
  "Handles any pre-processing of the schema, such as building a map of types for introspection."
  [root]
  (let [type-map (introspection/type-map root)]
    {:root
     (-> root
         (assoc-in [:fields :__type :type] (assoc introspection/FindType :solve (introspection/solve-type-by-name type-map)))
         (assoc-in [:fields :__schema :type] introspection/FindSchema)

         ;; TODO: only the "real" introspection fields (e.g. __type) should be available to the client.
         ;; These are here because here, they can conveniently be found by the introspection fields.
         (assoc-in [:fields :type :type] (assoc introspection/FindObjectType :solve (introspection/solve-type-by-object type-map)))
         (assoc-in [:fields :types :type] (assoc introspection/FindTypes :solve (introspection/solve-types type-map)))
         (assoc-in [:fields :queryType :type] (assoc introspection/FindQueryType :solve (introspection/solve-query-type root)))
         )}))


