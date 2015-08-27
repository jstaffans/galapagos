(ns galapagos.example.schema
  (:require [galapagos.schema :refer :all]))


(def AuthorType
  {:name        "Author"
   :description "The author of a blog post"
   :fields      {:id        {:type GraphQLInt}
                 :firstname {:type GraphQLString}
                 :lastname  {:type GraphQLString}}})

(declare PostType)

(def AuthorFindField
  {:description "Finds the author of a post"
   :arguments   {:post {:type PostType}}
   :fields      (:fields AuthorType)
   :solve       (fn [_]
                  {:id 123 :firstname "Some" :lastname "Author"})})


(def PostType
  {:name        "Post"
   :description "A blog post"
   :fields      {:id     {:type GraphQLInt :description "The ID"}
                 :title  {:type GraphQLString :description "The title"}
                 :author {:type AuthorFindField}}})


(def PostFindField
  {:description "Finds a post by id"
   :arguments   {:id {:type GraphQLInt}}
   :returns     PostType

   ; fields of find object are the same as for the type
   ; TODO: replace with pre-processing step based on :returns type
   :fields      (:fields PostType)

   :solve       (fn [{:keys [id]}]
                  (if (= 1 (Integer/valueOf id))
                    {:id 1 :title "Some post"}
                    nil))})

(def QueryRoot
  {:name        "QueryRoot"
   :description "The query root for this schema"
   :fields      {:post {:type PostFindField}}})


