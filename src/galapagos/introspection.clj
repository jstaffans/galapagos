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
;;
;; Galapagos eats its own dog food - introspection is defined as GraphQL types.

(schema/defenum TypeKind :SCALAR :OBJECT :INTERFACE :UNION :ENUM :INPUT_OBJECT :LIST :NON_NULL)

(schema/deftype TypeDescription []
  ;; TODO: missing fields (see spec)
  {:fields [:name schema/GraphQLString
            :kind TypeKind
            :description schema/GraphQLString
            :fields 'galapagos.introspection/FindFields

            ;; TODO
            :enumValues 'galapagos.introspection/FindEmptyList
            :inputFields 'galapagos.introspection/FindEmptyList
            :possibleTypes 'galapagos.introspection/FindEmptyList
            :interfaces 'galapagos.introspection/FindEmptyList
            ]})

(schema/defscalar UnknownValue nil)

(schema/definterface NotImplementedYet
  {:fields [:name UnknownValue
            :description UnknownValue
            :args UnknownValue
            :onOperation UnknownValue
            :onFragment UnknownValue
            :onField UnknownValue
            ]})

(schema/deftype SchemaDescription []
  ;; There are obviously some fields missing: :types and :queryType. These are handled by root
  ;; query fields that know how to get the types of a schema. It's done this way because the solve
  ;; functions are only available at runtime, after the type map has been built.
  {:fields [;; TODO
            :mutationType 'galapagos.introspection/FindEmpty
            :directives 'galapagos.introspection/FindEmptyList
            ]})

(schema/deftype FieldDescription []
  ;; As above, a :type field is missing. That information is only available at runtime.
  ;; TODO: other missing fields (see spec)
  {:fields [:name schema/GraphQLString
            :args 'galapagos.introspection/FindArgs
            :fields 'galapagos.introspection/FindFields

            ;; TODO
            :isDeprecated 'schema/GraphQLBoolean
            :deprecationReason 'schema/GraphQLString]})

(schema/deftype InputValueDescription []
  {:fields [:name schema/GraphQLString
            :description schema/GraphQLString]})


(schema/deftype DirectiveDescription []
  {:fields [:name schema/GraphQLString]})

(schema/deffield FindSchema :- SchemaDescription
  {:description "Finds the schema"
   :args        []
   :solve       (fn [_] (async/go (->SchemaDescription {})))})


;; Placeholders for the things that are not yet implemented
(schema/deffield FindEmpty :- NotImplementedYet
  {:description "Placeholder"
   :args        []
   :solve       (fn [_] (async/go {}))})

(schema/deffield FindEmptyList :- [NotImplementedYet]
  {:description "Placeholder"
   :args        []
   :solve       (fn [_] (async/go []))})


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

;; Also a skeleton which depends on a type map.
(schema/deffield FindTypes :- [TypeDescription]
  {:description "Finds all types belonging to a schema"
   :args        []})

;; Another skeleton
(schema/deffield FindQueryType :- TypeDescription
  {:description "Finds the query type of the schema"
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
                              (->FieldDescription
                                {:name name :isDeprecated false :deprecationReason ""})
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


(defn- build-type-definition
  "Creates a type definition map according to an internal format that is
  used by different introspection functions."
  [type metadata]
  (let [[type-name desc kind] ((juxt :name :description :kind) metadata)]
    (assoc type :__name type-name :description desc :__kind kind)))


(defn- build-type-description
  "Builds a TypeDescription from information gathered from type definition map."
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
      (if (nil? type-definition)
        (do
          (log/error "No definition for type" (keyword name) "found in type map!")
          (throw (IllegalArgumentException.)))
        (async/go (build-type-description type-definition))))))


(defn- first-arg-value
  [args]
  (first (vals args)))

(defn solve-type-by-object
  "Determines the type of an object. Solves to a type defined in a type map.
  See the `galapagos.introspection` namespace for the functions that handle building the type map."
  [type-map]
  (fn [args]
    (let [obj-desc (first-arg-value args)
          obj-name (-> obj-desc meta :introspection :name)
          type-definition (get type-map obj-name)]
      (if (nil? type-definition)
        (do
          (log/error "No definition for type" obj-name "found in type map!")
          (throw (IllegalArgumentException.)))
        (async/go (build-type-description type-definition))))))

(defn solve-query-type
  [root]
  (fn [_]
    (let [metadata (-> (meta root) :introspection)
          type-desc (build-type-description (build-type-definition root metadata))]
      (async/go type-desc))))

(defn solve-types
  "Solves to a list of all types available in a schema."
  [type-map]
  (fn [_]
    (async/go
      (mapv build-type-description (vals type-map)))))


(defn- register-type!
  "Stateful registration of a type. Besides normal information such as the description
  and the field map, we also register introspection metadata such as the *kind* of type."
  [types & {:keys [type metadata]}]
  (swap! types #(assoc % (:name metadata) (build-type-definition type metadata))))

(defn- walk-fields!
  "Recursively walks the fields of a type and registers any types found. It may happen
  that a type is registered multiple times, the previous registration then being overwritten.
  This only happens at startup though, not when an introspection query is performed,
  so the performance hit is negligible."
  [node types]
  (doseq [[_ field] (:fields node)]
    (let [type (if (coll? (:type field)) (:type field) {})
          metadata (:introspection (or (meta field) (meta type)))]
      (when (and type metadata)
        (register-type! types :type type :metadata (or (:of-type metadata) metadata)))))
  (doseq [[_ {:keys [type]}] (:fields node)]
    (walk-fields! type types)))


(defn- walk!
  "Registers interface types and then starts recursion of the fields of the root query type."
  [root types]
  (walk-fields! root types)

  ;; Register interfaces using the top-level map
  ;; TODO: get rid of this
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


