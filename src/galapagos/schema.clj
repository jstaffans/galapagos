(ns galapagos.schema
  (:require [schema.core :as s])
  (:import (schema.core Predicate))
  (:refer-clojure :exclude [deftype]))

(def GraphQLInt s/Int)

(def GraphQLFloat s/Num)

(def GraphQLString s/Str)

(defn primitive?
  [t]
  (or
    (= Class (type t))
    (= Predicate (type t))
    (:primitive? t)))

(defmacro defenum
  [name & values]
  `(def ~name
     {:primitive? true
      ;; TODO: do something with the values (validation)
      :values (vector ~@values)}))

(defmacro deftype
  [name t]
  `(def ~name (merge ~t {:name (str (quote ~name))})))

(defmacro deffield
  [name t]
  `(def ~name
     (merge
       (if-let [ret# (:returns ~t)]
         (if (vector? ret#)
           (assoc ~t :fields (:fields (first ret#)) :arity :many)
           (assoc ~t :fields (:fields ret#) :arity :one))
         ~t)
         {:name (str (quote ~name))})))


;M TODO: pre-processing
;; TODO: schema is not really needed for execution, but for validation and introspection it probably will be
(defn create-schema
  [root]
  {:schema root :root root})


