(ns galapagos.schema.blog
  (:require [galapagos.schema :as schema]
            [clojure.core.async :as async]))

(schema/defenum PreferredEditor :vim :emacs :sublime)

(schema/deftype Author
  {:fields {:id              {:type schema/GraphQLInt}
            :name            {:type schema/GraphQLString}
            :preferredEditor {:type PreferredEditor}}})

(declare Post)

(schema/deffield FindAuthor
  {:description "Finds the author of a post"
   :args        {:post Post}
   :returns     Author
   :solve       (fn [post]
                  (async/go
                    {:id 123 :name (str "Author Of " (:title post)) :preferredEditor :vim}))})

(schema/deftype Post
  {:description "A blog post"
   :fields      {:id     {:type schema/GraphQLInt :description "The ID"}
                 :title  {:type schema/GraphQLString :description "The title"}
                 :author {:type FindAuthor}}})

(schema/deffield FindPost
  {:description "Finds a post by id"
   :args        {:id schema/GraphQLInt}
   :returns     Post
   :solve       (fn [{:keys [id]}]
                  (async/go
                    (if (= 1 (Integer/valueOf id))
                      {:id 1 :title "Some post"}
                      nil)))})

(schema/deffield FindPosts
  {:description "Finds all posts"
   :args        {}
   :returns     [Post]
   :solve       (fn [_]
                  (async/go
                    [{:id 1 :title "Some post"}
                     {:id 2 :title "Another post"}]))})

(def QueryRoot
  {:name        "QueryRoot"
   :description "The query root for this schema"
   :fields      {:post  {:type FindPost}
                 :posts {:type FindPosts}}})

