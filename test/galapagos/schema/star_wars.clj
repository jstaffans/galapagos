(ns galapagos.schema.star-wars
  (:require [galapagos.schema :as schema]
            [clojure.core.async :as async]))


;; TODO: need interface support


(def QueryRoot
  {:name        "QueryRoot"
   :description "The query root for this schema"})

