(ns galapagos.schema-test
  (:require [clojure.test :refer :all]
            [juxt.iota :refer [given]]
            [schema.core :as s]
            [galapagos.schema :as schema]
            [schema.coerce :as coerce]))

(schema/deffield SomeField :- schema/GraphQLString
  {:args {:id  schema/GraphQLInt
          :ids [schema/GraphQLInt]
          (s/optional-key :optional) schema/GraphQLString}})

(deftest schema-test
  (testing "Coercion"
    (let [coercer (coerce/coercer {:arg1 schema/GraphQLInt :arg2 schema/GraphQLFloat} coerce/string-coercion-matcher)]
      (given (coercer {:arg1 "1" :arg2 "1.5"})
        :arg1 := 1
        :arg2 := 1.5)))

  (testing "Field argument metadata"
    (given (meta (:args SomeField))
      :id := {:type {:name :Int, :kind :SCALAR}, :required true}
      :ids := {:type {:name :List, :kind :LIST}, :required true}
      :optional := {:type {:name :String, :kind :SCALAR}, :required false})))



