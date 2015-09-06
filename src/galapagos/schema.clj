(ns galapagos.schema
  (:require [schema.core :as s])
  (:import (schema.core Predicate EnumSchema))
  (:refer-clojure :exclude [deftype definterface]))

(def GraphQLInt s/Int)

(def GraphQLFloat s/Num)

(def GraphQLString s/Str)

(def GraphQLScalar s/Any)

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

(defmacro definterface
  [name t]
  `(def ~name (merge ~t {:name (str (quote ~name))})))

(defmacro defunion
  [name ts]
  `(def ~name {:fields     (into {} (map :fields ~ts))
               :interfaces (mapcat :interfaces ~ts)}))

(defmacro deftype
  [name interfaces t]
  (let [interface-names (into [] (map str interfaces))]
    `(do
       (def ~name (merge ~t {:name       (str (quote ~name))
                             :interfaces (map keyword ~interface-names)}))
       (defn ~(symbol (str '-> name)) [v#] (with-meta v# {:type ~(keyword name)})))))

(defmacro deffield
  [name t]
  (let [ret (:returns t)
        [type arity] (if (vector? ret) [(first ret) :many] [ret :one])]
    `(def ~name
       (merge
         (assoc ~t :fields (:fields ~type) :type '~type :type-definition ~type :arity ~arity)
         {:name (str (quote ~name))}))))


;; TODO: pre-processing
;; TODO: schema is not really needed for execution, but for validation and introspection it probably will be
(defn create-schema
  [root]
  {:schema root :root root})


