(ns galapagos.query-test
  (:require [clojure.test :refer :all]
            [juxt.iota :refer [given]]
            [galapagos.query :as query]))


(deftest parse-test
  (testing "Query naming"
    (given (query/parse "query getPost { post { id } }")
      :op := :query
      :name := :getPost)

    (given (query/parse "{ post { id } }")
      :op := :query
      (partial keys) :⊅ [:name]))

  (testing "Fields"
    (given (query/parse "{ post { id, title } }")
      (comp first :fields) :⊃ {:name :post}
      (comp first :fields first :fields) :⊃ {:name :id}
      (comp second :fields first :fields) :⊃ {:name :title})))



