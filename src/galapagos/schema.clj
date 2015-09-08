(ns galapagos.schema
  (:require [schema.core :as s])
  (:import (schema.core Predicate EnumSchema))
  (:refer-clojure :exclude [deftype definterface]))

(def types (atom {}))

(def fields (atom {}))

;; ID can be an integer or an UUID
(def GraphQLID (s/if (partial re-matches #"^\d+$") s/Int s/Uuid))

(def GraphQLInt s/Int)

(def GraphQLFloat s/Num)

(def GraphQLString s/Str)

(def GraphQLBoolean s/Bool)

(defn update-types!
  [& args]
  (doseq [[type definition] (partition 2 args)]
    (swap! types #(assoc % type definition))))

(update-types!
  :ID {:kind :SCALAR}
  :Int {:kind :SCALAR}
  :Float {:kind :SCALAR}
  :String {:kind :SCALAR}
  :Boolean {:kind :SCALAR})

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
    `(do
       (def ~name ~kind)
       (update-types! ~(keyword name) {:kind :SCALAR}))))

(defmacro definterface
  [name t]
  `(do
     (def ~name ~t)
     (update-types! ~(keyword name) {:kind :INTERFACE})))

(defmacro defunion
  [name ts]
  `(do
     (def ~name {:fields     (into {} (map :fields ~ts))
                 :interfaces (mapcat :interfaces ~ts)})
     (update-types! ~(keyword name) {:kind :UNION})))

(defmacro deftype
  "Define a type corresponding to the GraphQL object type."
  [name interfaces t]
  (let [interface-names (into [] (map str interfaces))]
    `(do
       (def ~name (merge ~t {:interfaces (map keyword ~interface-names)}))
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

(defmacro defroot
  [name r]
  `(def ~name ~r))

;; TODO: can perform any pre-processing here
(defn create-schema
  [root]
  {:root root})


