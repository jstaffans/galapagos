(ns galapagos.schema
  (:require [galapagos.schema.helpers :as helpers]
            [galapagos.introspection :as introspection]
            [schema.core :as s]
            [clojure.core.async :as async]
            [medley.core :refer [map-vals]]
            [taoensso.timbre :as log])
  (:refer-clojure :exclude [deftype definterface]))

;; ## Schema
;;
;; Contains functions for defining the GraphQL schema that the application will support.
;; Basically all macros are syntactic sugar around normal var definitions.
;; Introspection is a big deal in GraphQL - this is supported by attaching
;; metadata to the defined vars.

(defmacro defscalar
  "Macro for defining the most basic GraphQL type - a scalar value."
  [name kind]
  (let [introspection-name (keyword (clojure.string/replace name "GraphQL" ""))]
    `(def
       ~(vary-meta name assoc :introspection {:name introspection-name :kind :SCALAR})
       ~kind)))

;; ### Built-in scalars

;; ID can be an integer or an UUID
(defscalar GraphQLID (s/if (partial re-matches #"^\d+$") s/Int s/Uuid))

(defscalar GraphQLInt s/Int)

(defscalar GraphQLFloat s/Num)

(defscalar GraphQLString s/Str)

(defscalar GraphQLBoolean s/Bool)


;; ### Enums, interfaces, unions and custom types

;; #### Introspection helpers

(defn- type-metadata
  "Get introspection information from var."
  [type-name]
  (-> type-name symbol resolve meta :introspection))

(defn- list-of
  [type]
  (assoc {} :name :List :kind :LIST :of-type type))

(defn- non-null-of
  [type]
  (assoc {} :name :Non-Null :kind :NON_NULL :of-type type))

(defn- field-metadata
  [field]
  (if (:var field)
    field
    (let [type-metadata (if (vector? (:type field))
                          (list-of (type-metadata (first (:type field))))
                          (type-metadata (:type field)))
          nullable-metadata (if (:required? field) (non-null-of type-metadata) type-metadata)]
      (with-meta field {:introspection nullable-metadata}))))

(defn- fields-with-introspection-metadata
  "Adds introspection information to individual fields. As GraphQL types are used
  for field definitions, we can get the introspection information from the vars."
  [fields]
  (map-vals field-metadata fields))

(defn- extract-introspection-metadata
  "Extracts the bits of a type definition that are interesting for introspection"
  [name m]
  (assoc {} :name (keyword name) :description (:description m)))


;; #### Type definition macros

(defmacro defenum
  [name & values]
  `(def
     ~(vary-meta name assoc :introspection (merge (extract-introspection-metadata name values) {:kind :ENUM}))
     (s/enum ~@values)))

(defmacro definterface
  "Define a GraphQL interface."
  [name t]
  (let [field-map (helpers/to-field-map (:fields t))
        fields-with-metadata (fields-with-introspection-metadata field-map)]
    `(def ~name
       (with-meta
         (merge ~t {:fields ~fields-with-metadata})
         {:introspection ~(merge (extract-introspection-metadata name t) {:kind :INTERFACE})}))))

(defmacro deftype
  "Define a type corresponding to the GraphQL object type."
  [name interfaces t]
  (let [interface-names (mapv str interfaces)
        qualified-interface-names (mapv #(str *ns* "/" %) interfaces)
        field-map (helpers/to-field-map (:fields t))
        fields-with-metadata (fields-with-introspection-metadata field-map)]
    `(do
       (def ~(vary-meta name assoc :introspection (merge (extract-introspection-metadata name t) {:kind :OBJECT}))
         (merge ~t
           {:interfaces (mapv keyword ~interface-names)}
           {:interface-definitions (mapv symbol ~qualified-interface-names)}
           {:fields ~fields-with-metadata}))
       (defn ~(symbol (str '-> name)) [v#] (with-meta v# {:type ~(keyword name)})))))

(defmacro defunion
  "Define a union of previously defined types."
  [name ts]
  `(def ~(vary-meta name assoc :introspection (merge (extract-introspection-metadata name {}) {:kind :UNION}))
     {:fields                (into {} (map :fields ~ts))
      :interfaces            (mapcat :interfaces ~ts)
      :interface-definitions (mapcat :interface-definitions ~ts)}))

(defmacro deffield
  "Defines a field that fetches something. The type will depend on what the field returns."
  [name s ret f]
  (if (= :- s)
    (let [[type arity] (if (vector? ret) [(first ret) :many] [ret :one])
          arg-map (helpers/to-arg-map (:args f))
          args-with-metadata (fields-with-introspection-metadata arg-map)]
      `(def ~(vary-meta name assoc :introspection (-> type symbol resolve meta :introspection))
         (merge ~f
           {:args ~args-with-metadata :returns ~ret :fields (:fields ~type) :type '~type :type-definition ~type :arity ~arity})))
    (throw (IllegalArgumentException. (str "Unknown schema definition operator: " s)))))

(defn- field-args
  "Gets the arguments map of a field. The arguments map may be defined in a referenced var."
  [f]
  (let [t (:type f)]
    (if (symbol? t)
      (-> t symbol find-var deref :args)
      (:args t))))

(defmacro defroot
  [name r]
  (let [field-map (helpers/to-field-map (:fields r))
        fields-with-metadata (fields-with-introspection-metadata field-map)]
    `(def ~name (merge ~r {:fields ~fields-with-metadata}))))

;; ### Schema utilities

(defn scalar?
  "Check if a a node's type is scalar or enum. Relies on introspection metadata."
  [node]
  (let [metadata (-> (meta node) :introspection)
        kind-scalar? (fn [kind] (contains? #{:ENUM :SCALAR} kind))
        kind (:kind metadata)]
    (or (kind-scalar? kind) (and (= :NON_NULL kind) (kind-scalar? (:kind (:of-type metadata)))))))

(defn parent-obj
  "Gets the parent object from function arguments."
  [args]
  (:__OBJ args))


;; ### Introspection types

(defenum TypeKind :SCALAR :OBJECT :INTERFACE :UNION :ENUM :INPUT_OBJECT :LIST :NON_NULL)

(deftype TypeDescription []
  ;; TODO: missing fields (see spec)
  {:fields [:name        GraphQLString
            :kind        TypeKind
            :description GraphQLString
            :fields      'galapagos.schema/FindFields]})

(deftype FieldDescription []
  ;; There is obviously a :type field missing. This is handled by a root query field
  ;; that knows how to get the type of a field. It's done this way because the solve
  ;; function is only available at runtime, after the type map has been built.
  ;; TODO: other missing fields (see spec)
  {:fields [:name   GraphQLString
            :args   'galapagos.schema/FindArgs
            :fields 'galapagos.schema/FindFields]})

(deftype InputValueDescription []
  {:fields [:name        GraphQLString
            :description GraphQLString]})

;; Skeleton field for finding a type. We can't solve anything before the type map
;; has been created, which is done in the `create-schema` function below. The `solve`
;; function is therefore added once the type map is available.
(deffield FindType :- TypeDescription
  {:description "Finds a type by name"
   :args        [:name GraphQLString :!]})

;; Likewise a skeleton field to which a `solve` method is associated once the
;; type map is known.
(deffield FindObjectType :- TypeDescription
  {:description "Finds the type of an object. For internal use only."
   :args        []})

(deffield FindFields :- [FieldDescription]
  {:description "Finds the fields belonging to a type"
   ;; TODO: :includeDeprecated doesn't actually do anything at the moment
   :args        [:includeDeprecated GraphQLBoolean]
   :solve       (fn [args]
                  (when (:includeDeprecated args) (log/warn "Field deprecation not supported yet!"))
                  (let [type-desc (parent-obj args)
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

(deffield FindArgs :- [InputValueDescription]
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

(defn- solve-type-by-name
  "Solves to a type defined in a type map. See the `galapagos.introspection` namespace
  for the functions that handle building the type map."
  [type-map]
  (fn [{:keys [name]}]
    (let [type-definition (get type-map (keyword name))]
      (async/go (build-type-description type-definition)))))


(defn- solve-type-by-object
  "Determines the type of an object. Solves to a type defined in a type map.
  See the `galapagos.introspection` namespace for the functions that handle building the type map."
  [type-map]
  (fn [args]
    (let [obj-desc (first (vals args))
          type-definition (get type-map (-> obj-desc meta :introspection :name))]
      (async/go (build-type-description type-definition)))))

(defn create-schema
  "Handles any pre-processing of the schema, such as building a map of types for introspection."
  [root]
  (let [type-map (introspection/type-map root)]
    {:root
     (-> root
         (assoc-in [:fields :__type :type] (assoc FindType :solve (solve-type-by-name type-map)))

         ;; TODO: only the "real" introspection fields (e.g. __type) should be available to the client
         (assoc-in [:fields :type :type] (assoc FindObjectType :solve (solve-type-by-object type-map))))}))


