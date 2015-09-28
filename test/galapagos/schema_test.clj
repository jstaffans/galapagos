(ns galapagos.schema-test
  (:require [clojure.test :refer :all]
            [juxt.iota :refer [given]]
            [schema.core :as s]
            [galapagos.schema :as schema]
            [galapagos.schema.helpers :as helpers]
            [schema.coerce :as coerce]))

(schema/deffield SomeField :- schema/GraphQLString
  {:args [:id       schema/GraphQLInt :!
          :ids      [schema/GraphQLInt] :!
          :optional schema/GraphQLString]})

(deftest schema-test
  (testing "Coercion"
    (let [coercer (coerce/coercer {:arg1 schema/GraphQLInt :arg2 schema/GraphQLFloat} coerce/string-coercion-matcher)]
      (given (coercer {:arg1 "1" :arg2 "1.5"})
        :arg1 := 1
        :arg2 := 1.5)))

  (testing "Field argument metadata"
    (let [args (:args SomeField)]
      (given (:id args)
        (comp :introspection meta) := {:name :Non-Null, :kind :NON_NULL, :of-type {:name :Int, :kind :SCALAR}})
      (given (:ids args)
        (comp :introspection meta) := {:name :Non-Null, :kind :NON_NULL, :of-type {:name :List, :kind :LIST, :of-type {:name :Int, :kind :SCALAR}}})
      (given (:optional args)
        (comp :introspection meta) := {:name :String, :kind :SCALAR})))

  (testing "Schema definition helpers"
    (let [[s1 s2] [(symbol "SomeType") (symbol "AnotherType")]]
      (given (helpers/to-field-map [:id s1 "Some description" :title s2 :!])
        :id := {:type s1 :description "Some description" :required? false}
        :title := {:type s2 :required? true}))))



