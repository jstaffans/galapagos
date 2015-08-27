(ns galapagos.schema-test
  (:require [clojure.test :refer :all]
            [juxt.iota :refer [given]]
            [galapagos.schema :as schema]
            [schema.coerce :as coerce]))

(deftest schema-test
  (testing "Coercion"
    (let [coercer (coerce/coercer {:arg1 schema/GraphQLInt :arg2 schema/GraphQLFloat} coerce/string-coercion-matcher)]
      (given (coercer {:arg1 "1" :arg2 "1.5"})
        :arg1 := 1
        :arg2 := 1.5))))



