(ns galapagos.introspection-test
  (:require [clojure.test :refer :all]
            [juxt.iota :refer [given]]
            [galapagos.introspection :as introspection]
            [galapagos.test-schemas.blog :refer [QueryRoot]]
            [galapagos.core :as core]))

(def blog-schema
  (core/create-schema QueryRoot))

(deftest introspection-test

  (testing "Type kinds"
    (given (introspection/type-map QueryRoot)
      :BlogUser :⊃ {:__kind :INTERFACE}
      :Blogger :⊃ {:__kind :UNION}
      :Post :⊃ {:__kind :OBJECT}
      :Author :⊃ {:__kind :OBJECT}
      :PublishingDate :⊃ {:__kind :SCALAR}
      :PreferredEditor :⊃ {:__kind :ENUM}

      ;; Builtin types
      :String :⊃ {:__kind :SCALAR}
      :Int :⊃ {:__kind :SCALAR}))

  (testing "Interface fields"
    (given
      (core/execute!! blog-schema
        "{ __type(name: BlogUser) { fields { name } } }")
      :data := {:__type {:fields [{:name :id} {:name :name} {:name :handle} {:name :friends}]}})
    (given
      (core/execute!! blog-schema
        "{ __type(name: BlogUser) { fields { args { name } } } }")
      :data := {:__type {:fields [{:args []} {:args []} {:args []} {:args [{:name :order}]}]}}))

  (testing "Nested queries"
    (given
      (core/execute!! blog-schema
        "{ __type(name: Post) { fields { name, type { name, kind } } } }")
      :data := {:__type {:fields
                         [{:name :id, :type {:name :Non-Null, :kind :NON_NULL}}
                          {:name :title, :type {:name :Non-Null, :kind :NON_NULL}}
                          {:name :date, :type {:name :PublishingDate, :kind :SCALAR}}
                          {:name :author, :type {:name :Author, :kind :OBJECT}}]}}))

  (testing "Input values"
    (given
      (core/execute!! blog-schema
        "{ __type(name: Author) { fields { args { name, type { name } } } } }")
      :data := {:__type {:fields [{:args []} {:args []} {:args [{:type {:name :String}, :name :size}]}]}}))

  (testing "Schema introspection"
    (given
      (core/execute!! blog-schema
        "{ __schema { types { name } } }")
      (comp set :types :__schema :data) := #{{:name :Post}
                                             {:name :Blogger}
                                             {:name :Author}
                                             {:name :String}
                                             {:name :Int}
                                             {:name :PublishingDate}
                                             {:name :PreferredEditor}
                                             {:name :Float}
                                             {:name :BlogUser}})
    (given
      (core/execute!! blog-schema
        "{ __schema { queryType { fields { name, args { name } } } } }")
      :data := {:__schema
                {:queryType
                 {:fields
                  [{:args [{:name :id}], :name :post}
                   {:args [], :name :posts}
                   {:args [{:name :handles}], :name :bloggers}
                   {:args [{:name :preferredEditor} {:name :rating}], :name :authors}
                   {:args [], :name :hello}]}}}))

  (testing "Of type"
    (given
      (core/execute!! blog-schema
        "{ __type(name: Author) { fields { type { ofType { name } } } } }"))))
