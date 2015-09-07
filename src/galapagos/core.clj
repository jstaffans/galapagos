(ns galapagos.core
  (:require [galapagos.query :refer [parse]]
            [galapagos.util :as util]
            [galapagos.schema :as schema]
            [clojure.core.async :as async]
            [schema.coerce :as coerce]
            [muse.core :as muse])
  (:refer-clojure :exclude [compile]))

(declare traverse traverse-root traverse-one traverse-many)

(defprotocol Solvable
  (solve [this value])
  (arity [this])
  (traverse-fn [this])
  (acc-fn [this]))

(defprotocol Visited
  (empty-node? [this])
  (done? [this]))

(defrecord SolvableRoot [node fields]
  Solvable
  (solve [_ _] (throw (IllegalStateException. "Root node should not be directly solved!")))
  (arity [_] :one)
  (traverse-fn [_] traverse-root)
  (acc-fn [_] #(assoc {} :data %))

  Visited
  (empty-node? [_] false)
  (done? [_] false))


(defn- resolve-name
  [query]
  (or (:alias query) (:name query)))

(defn- coerced-args
  [node query]
  (let [coercer (coerce/coercer (:args node) coerce/string-coercion-matcher)
        args    (or (:args query) {})
        coerced (coercer args)]
    (if-let [error-val (schema.utils/error-val coerced)]
      (throw (IllegalArgumentException. (str "Input argument coercion failed at " (:name query) " (" error-val ")")))
      coerced)))

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

  (arity [_] (:arity node))

  (traverse-fn [this]
    (if (= (arity this) :many) traverse-many traverse-one))

  ;; At solvable nodes, we introduce a new level of nesting
  (acc-fn [_]
    #(assoc {} (resolve-name query) %))

  Visited
  (empty-node? [_] (empty? fields))
  (done? [_] false))


;; A leaf that looks up keys in a parent map or record.
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

  (arity [_] :one)

  (traverse-fn [_] traverse-one)

  ;; At leaves, we use the result directly
  (acc-fn [_] #(into {} %))

  Visited
  (empty-node? [_] (empty? fields))
  (done? [_] true))


;; A leaf node that resolves directly to a value
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

  (arity [_] :one)

  (traverse-fn [_] traverse-one)

  (acc-fn [_] #(into {} %))

  Visited
  (empty-node? [_] false)
  (done? [_] true))


(defn- merge-children
  "Merges SolvableLookupField children to a single SolvableLookupField with multiple
   fields and key-names. Keeps traversal lean, because we don't have to visit each leaf
   separately.

   Example:

   [#SolvableLookupField{:fields [GraphQLInt], :key-names [{:name :id}]}
    #SolvableLookupField{:fields [GraphQLString], :key-names [{:name :fullname]}]

    -> [#SolvableLookupField{:fields [GraphQLInt GraphQLString], :key-names [{:name :id} {:name :fullname}]}]
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

(defn get-field-definition
  [root node query]
  (if-let [field (or
                   (get-in node [:fields (:name query) :type])
                   (first (keep
                            (fn [interface]
                              (get-in root [:interfaces interface :fields (:name query) :type]))
                            (-> node :type-definition :interfaces))))]
    field
    (throw (IllegalStateException. (str "Could not find definition for field " (:name query))))))

(defn- inline-fragment?
  [fragment]
  (not (keyword? fragment)))

(defn- attach-type-metadata
  [on field]
  (with-meta field {:on on}))

(defn- collect-fields
  "Collects fields defined in fragments and merges then with the selection fields.
  Fragment fields are tagged with :on metadata to distinguish them from selection-defined fields."
  [query fragments]
  (reduce (fn [acc fragment]
            (let [f (if (inline-fragment? fragment) fragment (get-in fragments [fragment]))]
              (into acc (mapv (partial attach-type-metadata (:on f)) (:fields f)))))
    (:fields query) (:fragments query)))

(defn- returns-primitive?
  [node]
  (and (:returns node) (schema/primitive? (:returns node))))

(defn- compile
  "Compiles query into a traversable graph."
  ([root query] (compile root root (:fragments query) (dissoc query :fragments)))
  ([root node fragments query]
   (let [fields (mapv (fn [query]
                        (let [field (get-field-definition root node query)]
                          (compile root field fragments query)))
                  (collect-fields query fragments))]
     (if (= root node)
       (->SolvableRoot node fields)

       (cond
         (schema/primitive? node)  (->SolvableLookupField [node] [query])
         (returns-primitive? node) (->SolvableRawField node query [node])
         :else                     (->SolvableNode node query (merge-children fields)))))))

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
        (if (or (empty? solution) (empty-node? field))
          (muse/value {})
          (traverse field solution)))
      (solve graph parent))))

(defn- traverse-many
  [graph field parent]
  ;; Solution is a collection of traversable nodes.
  ;; Use muse/traverse to iterate over all of them.
  (muse/traverse
    (fn [solution]
      (if (or (empty? solution) (empty-node? field))
        (muse/value {})
        (traverse field solution)))
    (solve graph parent)))

(defmulti merge-siblings (fn [graph _] (arity graph)))

(defmethod merge-siblings :one [_ muses] (into {} muses))

(defmethod merge-siblings :many [_ muses] (apply (partial map merge) muses))

(defn- traverse
  ([graph] (traverse graph {}))
  ([graph parent]
   (muse/fmap
     (acc-fn graph)
     (apply (partial muse/fmap
              (fn [& muses]
                ; merge the sibling muses into one map or list of maps
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

