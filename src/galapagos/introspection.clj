(ns galapagos.introspection
  (:require [clojure.core.async :as async]
            [galapagos.schema :as schema]
            [taoensso.timbre :as log]))

;; ## Introspection
;;
;; Talk about being meta! GraphQL can answer queries about itself, ie about its schema.
;; This is defined precisely in https://facebook.github.io/graphql/#sec-Schema-Introspection .
;; This namespace contains functions that build an overview map of the types used in the schema.


;; ### Introspection types

(schema/defenum TypeKind :SCALAR :OBJECT :INTERFACE :UNION :ENUM :INPUT_OBJECT :LIST :NON_NULL)

(schema/deftype TypeDescription []
  ;; TODO: missing fields (see spec)
  {:fields [:name        schema/GraphQLString
            :kind        TypeKind
            :description schema/GraphQLString
            :fields      'galapagos.introspection/FindFields]})

(schema/deftype FieldDescription []
  ;; There is obviously a :type field missing. This is handled by a root query field
  ;; that knows how to get the type of a field. It's done this way because the solve
  ;; function is only available at runtime, after the type map has been built.
  ;; TODO: other missing fields (see spec)
  {:fields [:name   schema/GraphQLString
            :args   'galapagos.introspection/FindArgs
            :fields 'galapagos.introspection/FindFields]})

(schema/deftype InputValueDescription []
  {:fields [:name        schema/GraphQLString
            :description schema/GraphQLString]})

;; Skeleton field for finding a type. We can't solve anything before the type map
;; has been created, which is done in the `create-schema` function below. The `solve`
;; function is therefore added once the type map is available.
(schema/deffield FindType :- TypeDescription
  {:description "Finds a type by name"
   :args        [:name schema/GraphQLString :!]})

;; Likewise a skeleton field to which a `solve` method is associated once the
;; type map is known.
(schema/deffield FindObjectType :- TypeDescription
  {:description "Finds the type of an object. For internal use only."
   :args        []})


(defn- field-args
  "Gets the arguments map of a field. The arguments map may be defined in a referenced var."
  [f]
  (let [t (:type f)]
    (if (symbol? t)
      (-> t symbol find-var deref :args)
      (:args t))))


(schema/deffield FindFields :- [FieldDescription]
  {:description "Finds the fields belonging to a type"
   ;; TODO: :includeDeprecated doesn't actually do anything at the moment
   :args        [:includeDeprecated schema/GraphQLBoolean]
   :solve       (fn [args]
                  (when (:includeDeprecated args) (log/warn "Field deprecation not supported yet!"))
                  (let [type-desc (schema/parent-obj args)
                        type-definition (:type-definition (meta type-desc))]
                    (async/go
                      (mapv
                        (fn [[name f]]
                          (let [metadata (assoc
                                           (:introspection (or (meta f) (meta (find-var (:var f)))))
                                           :args (field-args f))]
                            (with-meta
                              (->FieldDescription {:name name})
                              {:introspection metadata})))
                        (:fields type-definition)))))})

(schema/deffield FindArgs :- [InputValueDescription]
  {:description "Finds the arguments of a field"
   :args        []
   :solve       (fn [args]
                  (let [field-desc (get args :__OBJ)
                        args (-> field-desc meta :introspection :args)]
                    (async/go
                      (mapv
                        (fn [[name arg-meta]]
                          (with-meta
                            (->InputValueDescription
                              {:name         name
                               :description  "TODO: input value descriptions"
                               :defaultValue "TODO: default values"})
                            (meta arg-meta)))
                        args))))})


(defn- build-type-description
  "Builds a TypeDescription from information gathered from type map."
  [type-definition]
  (with-meta
    (->TypeDescription
      {:name        (:__name type-definition)
       :kind        (:__kind type-definition)
       :description (:description type-definition)})
    {:type-definition type-definition}))


(defn solve-type-by-name
  "Solves to a type defined in a type map. See the `galapagos.introspection` namespace
  for the functions that handle building the type map."
  [type-map]
  (fn [{:keys [name]}]
    (let [type-definition (get type-map (keyword name))]
      (async/go (build-type-description type-definition)))))


(defn solve-type-by-object
  "Determines the type of an object. Solves to a type defined in a type map.
  See the `galapagos.introspection` namespace for the functions that handle building the type map."
  [type-map]
  (fn [args]
    (let [obj-desc (first (vals args))
          type-definition (get type-map (-> obj-desc meta :introspection :name))]
      (async/go (build-type-description type-definition)))))


(defn- register-type!
  "Stateful registration of a type. Besides normal information such as the description
  and the field map, we also register introspection metadata such as the *kind* of type."
  [types & {:keys [type metadata]}]
  (let [[type-name desc kind] ((juxt :name :description :kind) metadata)]
    (swap! types #(assoc % type-name (assoc type :__name type-name :description desc :__kind kind)))))


(defn- walk-fields!
  "Recursively walks the fields of a type and registers any types found. It may happen
  that a type is registered multiple times, the previous registration then being overwritten.
  This only happens at startup though, not when an introspection query is performed,
  so the performance hit is negligible."
  [node types]
  (doseq [[_ field] (:fields node)]
    (let [type (if (coll? (:type field)) (:type field) {})
          metadata (:introspection (or (meta field) (meta type)))]
      (register-type! types :type type :metadata (or (:of-type metadata) metadata))))
  (doseq [[_ {:keys [type]}] (:fields node)]
    (walk-fields! type types)))


(defn- walk!
  "Registers interface types and then starts recursion of the fields of the root query type."
  [root types]
  (walk-fields! root types)

  ;; Register interfaces using the top-level map
  (doseq [interface (vals (:interfaces root))]
    (register-type! types
      :type interface
      :metadata (:introspection (meta interface)))))


(defn type-map
  "Returns a map of all types used in the given schema."
  [root]
  (let [types (atom {})]
    (walk! root types)
    @types))


