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
  `(def ~name
     (merge
       (if-let [ret# (:returns ~t)]
         (if (vector? ret#)
           (assoc ~t :fields (:fields (first ret#)) :arity :many)
           (assoc ~t :fields (:fields ret#) :arity :one))
         ~t)
         {:name (str (quote ~name))})))


(defrecord DataSource [field args]
  muse/DataSource
  (fetch [_]
    ((:solve field) args))

  muse/LabeledSource
  (resource-id [_] args))


;M TODO: pre-processing
;; TODO: schema is not really needed for execution, but for validation and introspection it probably will be
(defn create-schema
  [root]
  {:schema root :root root})


