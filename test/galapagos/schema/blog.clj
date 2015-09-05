(ns galapagos.schema.blog
  (:require [galapagos.schema :as schema]
            [clojure.core.async :as async]))

(schema/defenum PreferredEditor :vim :emacs :sublime)

(schema/definterface Blogger
  {:fields {:id   {:type schema/GraphQLInt}
            :name {:type schema/GraphQLString}}})

(schema/deftype Commenter [Blogger] {})

(schema/deftype Author [Blogger]
  {:fields {:preferredEditor {:type PreferredEditor}}})

(schema/deftype Post []
  {:description "A blog post"
   :fields      {:id    {:type schema/GraphQLInt :description "The ID"}
                 :title {:type schema/GraphQLString :description "The title"}
                 :date  {:type schema/GraphQLScalar :description "The publishing date"}}})

(schema/deffield FindPost
  {:description "Finds a post by id"
   :args        {:id schema/GraphQLInt}
   :returns     Post
   :solve       (fn [{:keys [id]}]
                  (async/go
                    ;; TODO: coercion
                    (if (< (Integer/valueOf id) 3)
                      {:id (Integer/valueOf id) :title (str "Post #" id)}
                      nil)))})


(schema/deffield FindPosts
  {:description "Finds all posts"
   :args        {}
   :returns     [Post]
   :solve       (fn [_]
                  (async/go
                    [{:id 1 :title "Some post"}
                     {:id 2 :title "Another post"}]))})

(schema/deffield FindAuthor
  {:description "Finds the author of a post"
   :args        {}
   :returns     Author
   :solve       (fn [args]
                  (async/go
                    {:id 123 :name (str "Author Of " (get-in args ['Post :title])) :preferredEditor :vim}))})

(schema/deffield FindProfilePicture
  {:description "Returns the profile picture of the desired size."
   :args        {:size schema/GraphQLString}
   :returns     schema/GraphQLString
   :solve       (fn [{:keys [size] :as args}]
                  (async/go (str "url/for/id/" (get-in args ['Author :id]) "?size=" (or size "default"))))})


(def QueryRoot
  {:name        "QueryRoot"
   :description "The query root for this schema"
   :fields      {:post           {:type FindPost}
                 :posts          {:type FindPosts}
                 ;; TODO: do something with applies-to
                 :author         {:type FindAuthor :applies-to [Post]}
                 :profilePicture {:type FindProfilePicture :applies-to [Author]}}

   ;; TODO: add this as pre-processing step
   :interfaces  {:Blogger Blogger}})

