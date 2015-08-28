(ns galapagos.example.schema
  (:require [galapagos.schema :as schema]))


(schema/defenum Publisher
  :packt :oreilly :springer)


(schema/deftype Author
  {:fields {:id        {:type schema/GraphQLInt}
            :name      {:type schema/GraphQLString}
            :publisher {:type Publisher}}})


(declare Post)


(schema/deffield FindAuthor
  {:description "Finds the author of a post"
   :arguments   {:post Post}
   :fields      (:fields Author)
   :solve       (fn [_]
                  {:id 123 :name "Some Author" :publisher :oreilly})})


(schema/deftype Post
  {:description "A blog post"
   :fields      {:id     {:type schema/GraphQLInt :description "The ID"}
                 :title  {:type schema/GraphQLString :description "The title"}
                 :author {:type FindAuthor}}})

(schema/deffield FindPost
  {:description "Finds a post by id"
   :arguments   {:id {:type schema/GraphQLInt}}
   :returns     Post

   ; fields of find object are the same as for the type
   ; TODO: replace with pre-processing step based on :returns type
   :fields      (:fields Post)

   :solve       (fn [{:keys [id]}]
                  (if (= 1 (Integer/valueOf id))
                    {:id 1 :title "Some post"}
                    nil))})

(def QueryRoot
  {:name        "QueryRoot"
   :description "The query root for this schema"
   :fields      {:post {:type FindPost}}})


