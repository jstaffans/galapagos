(ns galapagos.schema
  (:require [schema.core :as s]))

(def GraphQLInt s/Int)

(def GraphQLFloat s/Num)

(def GraphQLString s/Str)

; TODO: pre-processing

(defn create-schema
  [root]
  {:root root})


