(ns galapagos.schema
  (:require [galapagos.introspection :as introspection]
            [schema.core :as s]
            [clojure.core.async :as async]
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

(defn- fields-with-introspection-metadata
  "Adds introspection information to individual fields. As GraphQL types are used
  for field definitions, we can get the introspection information from the vars."
  [fields]
  (map-vals #(with-meta % {:introspection (-> (:type %) symbol resolve meta :introspection)}) fields))

(defn- extract-introspection-metadata
  "Extracts the bits of a type definition that are interesting for introspection"
  [name m]
  (assoc {} :name (keyword name) :description (:description m)))

(defmacro defenum
  [name & values]
  `(def
     ~(vary-meta name assoc :introspection (merge (extract-introspection-metadata name values) {:kind :ENUM}))
     (s/enum ~@values)))

(defmacro definterface
  "Define a GraphQL interface."
  [name t]
  (let [fields-with-type-names (fields-with-introspection-metadata (:fields t))]
    `(def ~name
       (with-meta
         (merge ~t {:fields ~fields-with-type-names})
         {:introspection ~(merge (extract-introspection-metadata name t) {:kind :INTERFACE})}))))

(defmacro deftype
  "Define a type corresponding to the GraphQL object type."
  [name interfaces t]
  (let [interface-names (into [] (map str interfaces))
        fields-with-metadata (fields-with-introspection-metadata (:fields t))]
    `(do
       (def ~(vary-meta name assoc :introspection (merge (extract-introspection-metadata name t) {:kind :OBJECT}))
         (merge ~t {:interfaces (map keyword ~interface-names)} {:fields ~fields-with-metadata}))
       (defn ~(symbol (str '-> name)) [v#] (with-meta v# {:type ~(keyword name)})))))

(defmacro defunion
  "Define a union of previously defined types."
  [name ts]
  `(def ~(vary-meta name assoc :introspection (merge (extract-introspection-metadata name {}) {:kind :UNION}))
     {:fields     (into {} (map :fields ~ts))
      :interfaces (mapcat :interfaces ~ts)}))

(defmacro deffield
  "Defines a field that fetches something. The type will depend on what the field returns."
  [name s ret f]
  (if (= :- s)
    (let [[type arity] (if (vector? ret) [(first ret) :many] [ret :one])]
      `(def ~(vary-meta name assoc
               :introspection (-> type symbol resolve meta :introspection))
         (merge
           (assoc ~f :fields (:fields ~type) :type '~type :type-definition ~type :arity ~arity)
           {:returns ~ret})))
    (throw (IllegalArgumentException. (str "Unknown schema definition operator: " s)))))

(defmacro defroot
  [name r]
  (let [fields-with-metadata (fields-with-introspection-metadata (:fields r))]
    `(def ~name (merge ~r {:fields ~fields-with-metadata}))))

;; ### Introspection types

(defenum TypeKind :SCALAR :OBJECT :INTERFACE :UNION :ENUM :INPUT_OBJECT :NON_NULL)

(deftype FieldDescription []
  {:fields {:name {:type GraphQLString}
            :kind {:type TypeKind}}})

(deffield FindFields :- [FieldDescription]
  {:description "Finds the fields belonging to a type"
   :args        {}
   :solve       (fn [args]
                  (let [type-desc (get args 'TypeDescription)
                        type-definition (:type-definition (meta type-desc))]
                    (async/go
                      (mapv
                        (fn [[name f]]
                          (let [metadata (:introspection (meta f))]
                            (->FieldDescription
                              {:name name
                               :kind (:kind metadata)})))
                        (:fields type-definition)))))})

(deftype TypeDescription []
  {:fields {:fields      {:type FindFields}
            :name        {:type GraphQLString}
            :kind        {:type GraphQLString}
            :description {:type GraphQLString}}})


;; Skeleton field for finding a type. We can't solve anything before the type map
;; has been created, which is done in the `create-schema` function below. The `solve`
;; function is therefore added once the type map is available.
(deffield FindType :- TypeDescription
  {:description "Finds a type by name"
   :args        {:name GraphQLString}})

(defn- solve-type
  "Solves to a type defined in a type map. See the `galapagos.introspection` namespace
  for the functions that handle building the type map."
  [type-map]
  (fn [{:keys [name]}]
    (async/go
      (let [type-definition (get type-map (keyword name))]
        (with-meta
          (->TypeDescription
            {:name        (:__name type-definition)
             :kind        (:__kind type-definition)
             :description (:description type-definition)})
          {:type-definition type-definition})))))

(defn create-schema
  "Handles any pre-processing of the schema, such as building a map of types for introspection."
  [root]
  (let [type-map (introspection/type-map root)]
    {:root (assoc-in root [:fields :__type :type] (assoc FindType :solve (solve-type type-map)))}))


;; ### Utilities

(defn scalar?
  "Check if a a node's type is scalar or enum. Relies on introspection metadata."
  [node]
  (contains? #{:ENUM :SCALAR} (-> (meta node) :introspection :kind)))

