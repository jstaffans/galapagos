(ns galapagos.schema.blog
  (:require [galapagos.schema :as schema]
            [schema.core :as s]
            [clojure.core.async :as async]))

(schema/defenum PreferredEditor :vim :emacs :sublime)

;; TODO: move syntax closer to Prismatic Schema (e.g. use :- operator)
;; TODO: accept Prismatic Schema schemas as types directly

(schema/definterface User
  {:fields {:id     {:type schema/GraphQLInt}
            :name   {:type schema/GraphQLString}
            :handle {:type schema/GraphQLString}}})

(schema/deftype Commenter [User]
  {:fields {:numComments {:type schema/GraphQLInt}}})

(schema/deftype Author [User]
  {:fields {:preferredEditor {:type PreferredEditor}}})

(schema/defunion Blogger [Commenter Author])

(schema/deftype Post []
  {:description "A blog post"
   :fields      {:id    {:type schema/GraphQLInt :description "The ID"}
                 :title {:type schema/GraphQLString :description "The title"}
                 :date  {:type schema/GraphQLScalar :description "The publishing date"}}})


(schema/deffield FindBloggers
  {:description "Finds bloggers by handles"
   :args        {:handles [schema/GraphQLString]}
   :returns     [Blogger]
   :solve       (fn [{:keys [handles]}]
                  (async/go
                    (mapv
                      #(if (re-matches #"^commenter.*" %)
                        (->Commenter {:id 200 :name "Commenter" :handle % :numComments 5})
                        (->Author {:id 300 :name "Author" :handle % :preferredEditor :vim}))
                      handles)))})

(schema/deffield FindPost
  {:description "Finds a post by id"
   :args        {:id schema/GraphQLInt}
   :returns     Post
   :solve       (fn [{:keys [id]}]
                  (async/go
                    (if (< id 3)
                      (->Post {:id id :title (str "Post #" id)})
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
                    (->Author {:id              123
                               :name            (str "Author Of " (get-in args ['Post :title]))
                               :preferredEditor :vim
                               :handle          (str "author-123")})))})

(schema/deffield FindProfilePicture
  {:description "Returns the profile picture of the desired size."
   :args        {(s/optional-key :size) schema/GraphQLString}
   :returns     schema/GraphQLString
   :solve       (fn [{:keys [size] :as args}]
                  (async/go (str "url/for/id/" (get-in args ['Author :id]) "?size=" (or size "default"))))})


(def QueryRoot
  {:name        "QueryRoot"
   :description "The query root for this schema"

   :fields      {:post           {:type FindPost}
                 :posts          {:type FindPosts}
                 :bloggers       {:type FindBloggers}
                 ;; TODO: do something with applies-to (validation)
                 :author         {:type FindAuthor :applies-to [Post]}
                 :profilePicture {:type FindProfilePicture :applies-to [Author]}}

   ;; TODO: add these as pre-processing step. Are unions needed?
   :interfaces  {:User User}
   :unions      {:Blogger Blogger}})

