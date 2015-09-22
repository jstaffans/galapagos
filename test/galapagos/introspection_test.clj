(ns galapagos.introspection-test
  (:require [clojure.test :refer :all]
            [juxt.iota :refer [given]]
            [galapagos.introspection :as introspection]
            [galapagos.schema.blog :refer [QueryRoot]]
            [galapagos.schema :as schema]
            [galapagos.core :as core]))

(def blog-schema
  (schema/create-schema QueryRoot))

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

  (testing "Nested queries"
    (given
      (core/execute!! blog-schema
        "{ __type(name: Post) { fields { name, type { name } } } }")
      :data) := {:__type {:fields
                          [{:name :id, :type {:name :Int, :kind :SCALAR}}
                           {:name :title, :type {:name :String, :kind :SCALAR}}
                           {:name :date, :type {:name :PublishingDate, :kind :SCALAR}}]}}))
