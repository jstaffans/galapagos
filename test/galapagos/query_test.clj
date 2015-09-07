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
      (comp second :fields first :fields) :⊃ {:name :title}))

  (testing "Named fields"
    (given (query/parse "{ somePost: post { id, title } }")
      (comp first :fields) :⊃ {:name :post :alias :somePost}
      (comp first :fields first :fields) :⊃ {:name :id}
      (comp second :fields first :fields) :⊃ {:name :title}))

  (testing "Siblings"
    (given (query/parse "{ somePost: post(id: 1) { title }, anotherPost: post(id: 2) { title } }")
      (comp first :fields) :⊃ {:name :post :alias :somePost}
      (comp second :fields) :⊃ {:name :post :alias :anotherPost}))

  (testing "Fragments"
    (given (query/parse "{ post { ... postFields } }")
      :fields := [{:name :post, :fields [], :fragments [:postFields]}])
    (given (query/parse "{ post { id, ... postFields } }")
      :fields := [{:name :post, :fields [{:name :id}], :fragments [:postFields]}])
    (given (query/parse "{ post { ... postFields } }
                         fragment postFields on Post { id, title }")
      :fields := [{:name :post, :fields [], :fragments [:postFields]}]
      :fragments := {:postFields {:on :Post :fields [{:name :id} {:name :title}], :fragments []}})
    (given (query/parse "{ post { id, ... on Post { title } } }")
      :fields := [{:name :post, :fields [{:name :id}], :fragments [{:on :Post :fields [{:name :title}], :fragments []}]}]))

  (testing "Input arguments"
    (given (query/parse "{ bloggers(id: 1, names: ['john', \"adam\"]) }")
      :fields := [{:name :bloggers :args {:id "1" :names ["john" "adam"]}}]))
  )



