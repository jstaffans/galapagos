(ns galapagos.core
  (:require [galapagos.query :refer [parse]]
            [galapagos.output :refer [collect]]
            [galapagos.schema :as schema]
            [clojure.core.async :as async]
            [muse.core :as muse])
  (:refer-clojure :exclude [compile]))

(defprotocol Solvable
  (solve [this value])
  (arity [this]))

(defrecord SolvableNode [node query fields]
  Solvable
  (solve [_ value]
    ; solve using either parent value or explicit argument
    (let [solution ((fnil (:solve node) value) (:args query))]
      (async/<!! solution)))
  (arity [_]
    (:arity node)))


(defrecord SolvableField [field query fields]
  Solvable
  (solve [_ value]
    ; only strategy is to get a property by name (works with maps and records)
    (get value (:name query)))
  (arity [_] :one))


(defn- compile-to-walkable
  "Compiles query into a walkable graph."
  [node query]
  (let [fields (mapv (fn [query]
                       (let [field (get-in node [:fields (:name query) :type])]
                         (compile-to-walkable field query)))
                 (:fields query))]
    (if (schema/primitive? node)
      (->SolvableField node query fields)
      (->SolvableNode node query fields))))

(defn- walk
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
                                       (mapv (partial walk field) solution)
                                       (walk field solution)))}}))
     (get-in context [:query :fields])
     (get-in context [:fields]))))

(defn execute
  "Main entry point."
  [{:keys [root]} query]
  (let [compile (partial compile-to-walkable root)]
    (->> query parse compile walk first collect)
    ; (->> query parse compile walk)
    ))


(comment
  ; Muse traversal

  (def graph
    {:node   galapagos.example.schema/FindPost
     :query  {:fields [{:name :post :args {:id 1}}]}
     :fields [{:field  schema.core/Str
               :query  {:fields [{:name :title}]}
               :fields []}]})

  (def nested-graph
    {:node   galapagos.example.schema/FindPost
     :query  {:fields [{:name :post :args {:id 1}}]}
     :fields [{:node   galapagos.example.schema/FindAuthor
               :query  {:fields [{:name :author :args {}}]}
               :fields [{:field  schema.core/Str
                         :query  {:fields [{:name :name}]}
                         :fields []}]}]})

  (defn traverse
    ([graph] (traverse graph [:solution {}]))
    ([graph [_ parent]]
     (let [query (some-> graph :query :fields first)
           args  (merge (:args query) parent)
           field (some-> graph :fields first)]
       (if (nil? field)
         (muse/value (select-keys parent [(:name query)]))
         (muse/traverse (fn [solution]
                          (muse/fmap #(assoc {} (:name query) %) (traverse field solution)))
           (muse/fmap #(assoc {} :solution %) (schema/->DataSource (:node graph) args)))))))



)