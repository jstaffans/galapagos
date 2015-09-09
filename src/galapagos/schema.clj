(ns galapagos.schema
  (:require [galapagos.introspection :as introspection]
            [schema.core :as s]
            [clojure.core.async :as async])
  (:import (schema.core Predicate EnumSchema))
  (:refer-clojure :exclude [deftype definterface]))

(def types (atom {}))

;; ID can be an integer or an UUID
(def GraphQLID (s/if (partial re-matches #"^\d+$") s/Int s/Uuid))

(def GraphQLInt s/Int)

(def GraphQLFloat s/Num)

(def GraphQLString s/Str)

(def GraphQLBoolean s/Bool)

(def builtin-types
  {
   :ID      {:kind :SCALAR}
   :Int     {:kind :SCALAR}
   :Float   {:kind :SCALAR}
   :String  {:kind :SCALAR}
   :Boolean {:kind :SCALAR}})

;; TODO: pretty crude way of determining if something is one of the above
(defn primitive?
  [t]
  (or
    (= Class (type t))
    (= Predicate (type t))
    (= EnumSchema (type t))))

(defmacro defenum
  [name & values]
  `(def ~name
     (s/enum ~@values)))

(defmacro defscalar
  [name kind]
  (let [k (keyword name)]
    `(def ~name ~kind)))

(defmacro definterface
  [name t]
  `(def ~name ~t))

(defmacro defunion
  [name ts]
  `(def ~name {:fields     (into {} (map :fields ~ts))
               :interfaces (mapcat :interfaces ~ts)}))

(defmacro deftype
  "Define a type corresponding to the GraphQL object type."
  [name interfaces t]
  (let [interface-names (into [] (map str interfaces))]
    `(do
       (def ~name (with-meta
                    (merge ~t {:interfaces (map keyword ~interface-names)})
                    {:introspection
                     {:is-type? true
                      :name ~(keyword name)
                      :kind :OBJECT}}))
       (defn ~(symbol (str '-> name)) [v#] (with-meta v# {:type ~(keyword name)})))))

(defmacro deffield
  "Defines a field that fetches something."
  [name s ret f]
  (if (= :- s)
    (let [[type arity] (if (vector? ret) [(first ret) :many] [ret :one])]
      `(def ~name
         (merge
           (assoc ~f :fields (:fields ~type) :type '~type :type-definition ~type :arity ~arity)
           {:returns ~ret})))
    (throw (IllegalArgumentException. (str "Unknown schema definition operator: " s)))))

;; Introspection types

(defenum TypeKind :SCALAR :OBJECT :INTERFACE :UNION :ENUM :INPUT_OBJECT :NON_NULL)

(deftype TypeDescription []
  {:fields {:kind        {:type TypeKind}
            :name        {:type GraphQLString}
            :description {:type GraphQLString}}})


(deffield FindType :- TypeDescription
  {:description "Finds a type by name"
   :args {:name GraphQLString}
   ; solve added once we have the root
   })

(defn- solve-type
  [type-map]
  (fn [{:keys [name]}]
    (async/go
      (let [type-definition (get type-map (keyword name))]
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


