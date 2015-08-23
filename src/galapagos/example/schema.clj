(ns galapagos.example.schema
  (:require [galapagos.schema :refer :all]
            [schema.core :as s]))


(def AuthorType
  {:name        "Author"
   :description "The author of a blog post"
   :fields      {:id        {:type s/Int}
                 :firstname {:type s/Str}
                 :lastname  {:type s/Str}}})

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
   :fields      {:id     {:type s/Int :description "The ID"}
                 :title  {:type s/Str :description "The title"}
                 :author {:type AuthorFindField}}})


(def PostFindField
  {:description "Finds a post by id"
   :arguments   {:id {:type s/Int}}
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


