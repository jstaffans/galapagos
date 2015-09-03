(ns galapagos.schema
  (:require [schema.core :as s]
            [muse.core :as muse])
  (:import (schema.core Predicate EnumSchema))
  (:refer-clojure :exclude [deftype]))

(def GraphQLInt s/Int)

(def GraphQLFloat s/Num)

(def GraphQLString s/Str)

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

(defmacro deftype
  [name t]
  `(def ~name (merge ~t {:name (str (quote ~name))})))

(defmacro deffield
  [name t]
  (let [ret (:returns t)
        [type arity] (if (vector? ret) [(first ret) :many] [ret :one])]
    `(def ~name
       (merge
         (assoc ~t :fields (:fields ~type) :type '~type :arity ~arity)
         {:name (str (quote ~name))}))))


;; TODO: pre-processing
;; TODO: schema is not really needed for execution, but for validation and introspection it probably will be
(defn create-schema
  [root]
  {:schema root :root root})


