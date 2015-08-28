(ns galapagos.schema
  (:require [schema.core :as s])
  (:import (schema.core Predicate)))

(def GraphQLInt s/Int)

(def GraphQLFloat s/Num)

(def GraphQLString s/Str)

(defn primitive?
  [t]
  (= Predicate (type t)))

; TODO: pre-processing

(defn create-schema
  [root]
  {:root root})


