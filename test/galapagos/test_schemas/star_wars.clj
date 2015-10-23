(ns galapagos.test-schemas.star-wars
  (:require [galapagos.schema :as schema]
            [clojure.core.async :as async]))

(def data
  (atom
    {1000 {:name        "Luke Skywalker"
           :friends     [1002 1003 2000 2001]
           :appears-in  [:a-new-hope :empire-strikes-back :return-of-the-jedi]
           :home-planet "Tatooine"}
     1001 {:name        "Vader"
           :friends     [1004]
           :appears-in  [:a-new-hope :empire-strikes-back :return-of-the-jedi]
           :home-planet "Tatooine"}
     1002 {:name       "Han Solo"
           :friends    [1000 1003 2001]
           :appears-in [:a-new-hope :empire-strikes-back :return-of-the-jedi]}
     1003 {:name       "Leia"
           :friends    [1000 1002 2000 2001]
           :appears-in [:a-new-hope :empire-strikes-back :return-of-the-jedi]}
     1004 {:name       "Wilhuff Tarkin"
           :friends    [1001]
           :appears-in [:a-new-hope]}

     2000 {:name             "C-3PO"
           :friends          [1000 1002 1003 2001]
           :appears-in       [:a-new-hope :empire-strikes-back :return-of-the-jedi]
           :primary-function "Protocol"}
     2001 {:name             "R2-D2"
           :friends          [1000 1002 1003]
           :appears-in       [:a-new-hope :empire-strikes-back :return-of-the-jedi]
           :primary-function "Astromech"}}))


(schema/defenum Episode :a-new-hope :empire-strikes-back :return-of-the-jedi)

;; interface Character {
;;   id: String!
;;   name: String
;;   friends: [Character]
;;   appearsIn: [Episode]
;; }

(declare FindFriends FindAppearsIn)

(schema/definterface MovieCharacter
  {:fields [:id schema/GraphQLInt :! "The unique ID of this character"
            :name schema/GraphQLString "The character's name"
            :friends   #'FindFriends           "This character's friends"
            ;:appearsIn #'FindAppearsIn         "Which episodes this character appears in"
            ]})

(schema/deftype Human [MovieCharacter]
  {:fields [:home-planet schema/GraphQLString "The character's home planet"]})

(schema/deftype Droid [MovieCharacter]
  {:fields [:primary-function schema/GraphQLString "The character's primary function"]})

(defn- character-factory-fn
  [id]
  (if (< id 2000) ->Human ->Droid))

(schema/deffield FindCharacter :- MovieCharacter
  {:description "Finds a character by ID"
   :args        [:id schema/GraphQLInt :!]
   :solve       (fn [args]
                  (async/go (get @data (:id args))))})

(schema/deffield FindFriends :- [MovieCharacter]
  {:description "Finds the friends of a character"
   :args        []
   :solve       (fn [args]
                  (let [character (schema/it args)]
                    (let [ids (get-in @data [(:id character) :friends])]
                      (async/go
                        (mapv
                          (fn [friend-id]
                            (let [friend (get @data friend-id)]
                              ((character-factory-fn friend-id) friend)))
                          ids)))))})

(schema/defroot QueryRoot
  {:fields [:character FindCharacter]
   :interfaces {:MovieCharacter #'MovieCharacter}})

