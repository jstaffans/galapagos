(ns galapagos.core
  (:require [galapagos.query :refer [parse]]
            [galapagos.schema :as schema]
            [clojure.core.async :as async]
            [muse.core :as muse])
  (:refer-clojure :exclude [compile]))

(defprotocol Solvable
  (solve [this value])
  (arity [this]))

(defprotocol Visited
  (done? [this]))

;; Wraps the solution as a muse DataSource
(defrecord SolvableMuseNode [node query fields]
  Solvable
  (solve [_ value]
    (let [args (merge (:args query) value)]
      (schema/->DataSource node args)))

  (arity [_]
    (:arity node))

  Visited
  (done? [_] false))

(defrecord SolvableField [field query fields]
  Solvable
  (solve [_ value]
    ; only strategy is to get a property by name (works with maps and records)
    (get value (:name query)))

  (arity [_] :one)

  Visited
  (done? [_] true))

;; Muse wrapper for SolvableField
(defrecord FieldDataSource [field parent]
  muse/DataSource
  (fetch [_]
    (async/go (solve field parent)))

  muse/LabeledSource
  (resource-id [_] parent))


(defn- compile
  "Compiles query into a traversable graph."
  ([root query] (compile root root query))
  ([root node query]
   (let [fields (mapv (fn [query]
                        (let [field (get-in node [:fields (:name query) :type])]
                          (compile root field query)))
                  (:fields query))]
     ;; Don't return the query root of the schema, instead return
     ;; the first subtree (the actual query) which is easier to traverse.
     (if (= root node)
       (first fields)

       (if (schema/primitive? node)
         (->SolvableField node [(:name query)])
         (->SolvableMuseNode node query fields))))))

;; deprecated for execution - might be a good approach for introspection though
(defn- walk
  "Walks the graph, providing solutions as inputs to child elements.
   The result is a solved context. This is a naive approach and not
   as efficient as traversal with muse (see the traverse function)."
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


(declare traverse)

(defn traverse-one
  [graph fields parent]
  (if (done? graph)
    (->FieldDataSource graph parent)

    (muse/flat-map
      (fn [solution]
        ;; TODO: support multiple fields
        (traverse (first fields) solution))
      (solve graph parent))))

(defn traverse-many
  [graph field parent]
  (muse/traverse
    (fn [solution]
      (traverse field solution))
    (solve graph parent)))

(defn traverse
  ([graph] (traverse graph {}))
  ([graph parent]
   (let [query (:query graph)
         fields (:fields graph)]
     (muse/fmap #(assoc {} (:name query) %)
       (if (= (arity graph) :one)
         (traverse-one graph fields parent)
         (traverse-many graph fields parent))))))

(defn execute
  "Main entry point."
  [{:keys [root]} query-string]
  (let [query (parse query-string)
        graph (compile root query)]
    (traverse graph)))


;; Example graphs that can be traversed with muse
(comment
  ;; (muse/run!! (traverse graph))
  (def graph
    (galapagos.core/map->SolvableMuseNode
      {:node   galapagos.example.schema/FindPost
       :query  {:name :post :args {:id 1}}
       :fields [(galapagos.core/map->SolvableField
                  {:field  galapagos.schema/GraphQLString
                   :query  {:name :title}
                   :fields []})]}))


  ;; TODO: make this work
  ;; (muse/run!! (traverse graph-with-multiple-leaves))
  (def graph-with-multiple-leaves
    (galapagos.core/map->SolvableMuseNode
      {:node   galapagos.example.schema/FindPost
       :query  {:name :post :args {:id 1}}
       :fields [(galapagos.core/map->SolvableField
                  {:field  galapagos.schema/GraphQLInt
                   :query  {:name :id}
                   :fields []})
                (galapagos.core/map->SolvableField
                  {:field  galapagos.schema/GraphQLString
                   :query  {:name :title}
                   :fields []})]}))

  ;; (muse/run!! (traverse graph-with-list))
  (def graph-with-list
    (galapagos.core/map->SolvableMuseNode
      {:node   galapagos.example.schema/FindPosts
       :query  {:name :posts :args {}}
       :fields [(galapagos.core/map->SolvableField
                  {:field  galapagos.schema/GraphQLString
                   :query  {:name :title}
                   :fields []})]}))

  ;; (muse/run!! (traverse nested-graph))
  (def nested-graph
    (galapagos.core/map->SolvableMuseNode
      {:node   galapagos.example.schema/FindPost
       :query  {:name :post :args {:id 1}}
       :fields [(galapagos.core/map->SolvableMuseNode
                  {:node   galapagos.example.schema/FindAuthor
                   :query  {:name :author :args {}}
                   :fields [(galapagos.core/map->SolvableField
                              {:field  galapagos.schema/GraphQLString
                               :query  {:name :name}
                               :fields []})]})]})))