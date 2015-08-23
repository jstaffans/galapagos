(ns galapagos.core
  (:require [galapagos.query :as query]
            [galapagos.output :as output]
            [plumbing.core :refer [fnk]])
  (:refer-clojure :exclude [compile]))

(defprotocol Solvable
  (solve [this value]))

(defrecord SolvableNode [node query fields]
  Solvable
  (solve [_ _]
    ((:solve node) (:args query))))

(defrecord SolvableField [field query fields]
  Solvable
  (solve [_ value]
    ; TODO: only strategy is to get a property by name (works with maps and records)
    (get value (:name query))))

(defn compile
  "Compiles query into a walkable graph."
  [graph node query]
  (let [fields (mapv (fn [query]
                       (let [field (get-in node [:fields (:name query) :type])]
                         (compile graph field query)))
                 (:fields query))]

    ; TODO: only supporting primitive leaves currently
    (if (and (map? node) (contains? node :fields))
      (->SolvableNode node query fields)
      (->SolvableField node query fields))))

;; TODO: defer executions using Muse or similar
(defn walk
  "Walks the graph, providing solutions as inputs to child elements.
   The result is a solved context."
  ([context] (walk context {}))
  ([context parent-solution]
   (mapv
     (fn [query field]
       (let [solution (solve field parent-solution)]
         {(:name query) {:solution solution
                         :fields   (walk field solution)}}))
     (get-in context [:query :fields])
     (get-in context [:fields]))))

(defn execute
  [graph root query-string]
  (->> query-string
       query/parse
       (compile graph root)
       walk
       first                                                ; TODO: multiple query contexts?
       output/collect))


