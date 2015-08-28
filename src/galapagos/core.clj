(ns galapagos.core
  (:require [galapagos.query :as query]
            [galapagos.output :as output]
            [galapagos.schema :as schema]
            [plumbing.core :refer [fnk]]
            [schema.core :as s])
  (:refer-clojure :exclude [compile]))

(defprotocol Solvable
  (solve [this value])
  (arity [this]))

(defrecord SolvableNode [node query fields]
  Solvable
  (solve [_ value]
    ; solve using either parent value or explicit argument
    ((fnil (:solve node) value) (:args query)))
  (arity [_]
    (:arity node)))

(defrecord SolvableField [field query fields]
  Solvable
  (solve [_ value]
    ; only strategy is to get a property by name (works with maps and records)
    (get value (:name query)))
  (arity [_] :one))

(defn compile
  "Compiles query into a walkable graph."
  [graph node query]
  (let [fields (mapv (fn [query]
                       (let [field (get-in node [:fields (:name query) :type])]
                         (compile graph field query)))
                 (:fields query))]

    (if (schema/primitive? node)
      (->SolvableField node query fields)
      (->SolvableNode node query fields))))

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
                         :fields   (when (not (nil? solution))
                                     (if (= (arity field) :many)
                                       (mapv #(walk field %) solution)
                                       (walk field solution)))}}))
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


