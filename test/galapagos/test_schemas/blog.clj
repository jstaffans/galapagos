(ns galapagos.test-schemas.blog
  (:require [galapagos.schema :as schema]
            [schema.core :as s]
            [clojure.core.async :as async]))

(schema/defenum PreferredEditor :VIM :EMACS :JOE)

(declare FindFriends FindProfilePicture FindAuthor)

(schema/definterface BlogUser
  {:fields [:id      schema/GraphQLString
            :name    schema/GraphQLString
            :handle  schema/GraphQLString
            :friends #'FindFriends]})

(schema/deftype Commenter [BlogUser]
  {:fields [:numComments schema/GraphQLInt]})

(schema/deffield FindProfilePicture :- schema/GraphQLString
  {:description "Returns the profile picture of the desired size."
   :args        [:size schema/GraphQLString]
   :solve       (fn [{:keys [size] :as args}]
                  (async/go (str "url/for/id/" (:id (schema/it args)) "?size=" (or size "default"))))})


(schema/deftype Author [BlogUser]
  {:fields [:preferredEditor PreferredEditor :!
            :averageRating   schema/GraphQLFloat :!
            :profilePicture  #'FindProfilePicture]})

(schema/deffield FindAuthor :- Author
  {:description "Finds the author of a post"
   :args        []
   :solve       (fn [args]
                  (async/go
                    (->Author {:id              123
                               :name            (str "Author Of " (:title (schema/it args)))
                               :preferredEditor :VIM
                               :handle          (str "author-123")})))})


(schema/deffield FindAuthors :- [Author]
  {:description "Finds authors by preferred editor"
   :args        [:preferredEditor PreferredEditor :!
                 :rating          schema/GraphQLFloat]

   :solve       (fn [{:keys [preferredEditor rating]}]
                  (async/go
                    [(->Author {:id              256
                                :name            (str "Author Who Likes " (name preferredEditor))
                                :preferredEditor preferredEditor
                                :handle          (str "author-256")})]))})

(schema/defenum FriendsOrder :ASC :DESC)

(schema/deffield FindFriends :- [BlogUser]
  {:description "Finds the friends of a blog user"
   :args        [:order FriendsOrder]
   :solve       (fn [{:keys [order] :as args}]
                  (async/go
                    [(->Author {:id   512
                                :name (str "Friend Of " (:name (schema/it args)) " " order)})]))})


(schema/defunion Blogger [Commenter Author])

(schema/defscalar PublishingDate s/Inst)

(schema/deftype Post []
  {:description "A blog post"
   :fields      [:id     schema/GraphQLInt    :!  "The ID"
                 :title  schema/GraphQLString :!  "The title"
                 :date   PublishingDate
                 :author #'FindAuthor]})

(schema/deffield FindBloggers :- [Blogger]
  {:description "Finds bloggers by handles"
   :args        [:handles [schema/GraphQLString]]
   :solve       (fn [{:keys [handles]}]
                  (async/go
                    (mapv
                      #(if (re-matches #"^commenter.*" %)
                        (->Commenter {:id 200 :name "Commenter" :handle % :numComments 5})
                        (->Author {:id 300 :name "Author" :handle % :preferredEditor :VIM}))
                      handles)))})

(schema/deffield FindPost :- Post
  {:description "Finds a post by id"
   :args        [:id schema/GraphQLInt]
   :solve       (fn [{:keys [id]}]
                  (async/go
                    (if (< id 3)
                      (->Post {:id id :title (str "Post #" id)})
                      nil)))})


(schema/deffield FindPosts :- [Post]
  {:description "Finds all posts"
   :args        {}
   :solve       (fn [_]
                  (async/go
                    [{:id 1 :title "Some post"}
                     {:id 2 :title "Another post"}]))})

(schema/deffield FindHelloWorld :- schema/GraphQLString
  {:args  []
   :solve (fn [_] (async/go "World!"))})


(schema/defroot QueryRoot
  {:description "The query root for this schema"

   :fields      [:post FindPost
                 :posts FindPosts
                 :bloggers FindBloggers
                 :authors FindAuthors
                 :hello FindHelloWorld]})

