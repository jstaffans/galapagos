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
  `(def
     ~(vary-meta name assoc :introspection {:name (keyword name) :kind :SCALAR})
     ~kind))

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

(defmacro defenum
  [name & values]
  `(def
     ~(vary-meta name assoc :introspection {:name (keyword name) :kind :ENUM})
     (s/enum ~@values)))

(defmacro definterface
  "Define a GraphQL interface."
  [name t]
  (let [fields-with-type-names (fields-with-introspection-metadata (:fields t))]
    `(def ~name (merge ~t {:fields ~fields-with-type-names}))))

(defmacro deftype
  "Define a type corresponding to the GraphQL object type."
  [name interfaces t]
  (let [interface-names (into [] (map str interfaces))
        fields-with-type-names (fields-with-introspection-metadata (:fields t))]
    `(do
       (def ~(vary-meta name assoc :introspection {:name (keyword name) :kind :OBJECT})
         (merge ~t {:interfaces (map keyword ~interface-names)} {:fields ~fields-with-type-names}))
       (defn ~(symbol (str '-> name)) [v#] (with-meta v# {:type ~(keyword name)})))))

(defmacro defunion
  "Define a union of previously defined types."
  [name ts]
  `(def ~(vary-meta name assoc :introspection {:name (keyword name) :kind :UNION})
     {:fields     (into {} (map :fields ~ts))
      :interfaces (mapcat :interfaces ~ts)}))

(defmacro deffield
  "Defines a field that fetches something. The type will depend on what the field returns."
  [name s ret f]
  (if (= :- s)
    (let [[type arity] (if (vector? ret) [(first ret) :many] [ret :one])]
      `(def ~(vary-meta name assoc
               :introspection {:kind (-> type symbol resolve meta :introspection :kind)})
         (merge
           (assoc ~f :fields (:fields ~type) :type '~type :type-definition ~type :arity ~arity)
           {:returns ~ret})))
    (throw (IllegalArgumentException. (str "Unknown schema definition operator: " s)))))

;; ### Introspection types

(defenum TypeKind :SCALAR :OBJECT :INTERFACE :UNION :ENUM :INPUT_OBJECT :NON_NULL)

(deftype FieldDescription []
  {:fields {:name {:type GraphQLString}}})

(deffield FindFields :- [FieldDescription]
  {:description "Finds the fields belonging to a type"
   :args {}
   :solve (fn [args]
            (println (get args 'TypeDescription))
            (async/go [(->FieldDescription {:name "Foobar"})]))})

(deftype TypeDescription []
  {:fields {:fields      {:type FindFields}
            :name        {:type GraphQLString}
            :description {:type GraphQLString}}})


(deffield FindType :- TypeDescription
  {:description "Finds a type by name"
   :args        {:name GraphQLString}
   ; solve added once we have the root
   })

(defn- solve-type
  [type-map]
  (fn [{:keys [name]}]
    (async/go
      (let [type-definition (get type-map #spy/p (keyword name))]
        (clojure.pprint/pprint (keys type-map))
        (println type-definition)
        (->TypeDescription
          {:name        (:name type-definition)
           :description (:description type-definition)})))))

(defmacro defroot
  [name r]
  `(def ~name ~r))

(defn- build-type-map
  [root]
  (let [types (introspection/walk root (atom {}))]
    (reduce-kv (fn [acc name type] (assoc acc name type)) {} @types)))

;; TODO: can perform any pre-processing here
(defn create-schema
  [root]
  (let [type-map (build-type-map root)]
    {:root (assoc-in root [:fields :__type :type] (assoc FindType :solve (solve-type type-map)))}))


;; ### Utilities

(defn scalar?
  "Check if a a node's type is scalar or enum. Relies on introspection metadata."
  [node]
  (contains? #{:ENUM :SCALAR} (-> (meta node) :introspection :kind)))

