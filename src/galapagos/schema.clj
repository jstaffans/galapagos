(ns galapagos.schema
  (:require [galapagos.schema.helpers :as helpers]
            [schema.core :as s]
            [medley.core :refer [map-vals]])
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

    `(def ~(vary-meta name assoc :introspection (merge (extract-introspection-metadata name t) {:kind :INTERFACE}))
      (merge ~t {:fields ~fields-with-metadata}))))

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
          type-metadata (-> type symbol resolve meta :introspection)
          returns (if (= arity :many) (list-of type-metadata) type-metadata)
          arg-map (helpers/to-arg-map (:args f))
          args-with-metadata (fields-with-introspection-metadata arg-map)]
      `(def ~(vary-meta name assoc :introspection type-metadata)
         (merge ~f
           {:args ~args-with-metadata :returns ~returns :fields (:fields ~type) :type '~type :type-definition ~type :arity ~arity})))
    (throw (IllegalArgumentException. (str "Unknown schema definition operator: " s)))))

(defmacro defroot
  "Defines the query root of the schema."
  [name r]
  (let [field-map (helpers/to-field-map (:fields r))
        fields-with-metadata (fields-with-introspection-metadata field-map)]
    `(def ~name
       ;; This is the only place where the metadata is attached to the value, not the var itself.
       ;; It makes it easier to pass around the root (QueryRoot instead of #'QueryRoot).
       (with-meta
         (merge ~r {:fields ~fields-with-metadata})
         {:introspection {:name :QueryRoot :kind :OBJECT}}))))

;; ### Schema utilities

(defn scalar?
  "Check if a a node's type is scalar or enum. Relies on introspection metadata."
  [node]
  (let [metadata (-> (meta node) :introspection)
        kind-scalar? (fn [kind] (contains? #{:ENUM :SCALAR} kind))
        kind (:kind metadata)]
    (or (kind-scalar? kind) (and (= :NON_NULL kind) (kind-scalar? (:kind (:of-type metadata)))))))

(defn recursive?
  [node]
  (nil? (-> node :type :arity)))

(def it-key :__OBJ)

(defn it
  "Gets the parent instance from function arguments."
  [args]
  (get args it-key))

