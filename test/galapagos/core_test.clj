(ns galapagos.core-test
  (:require [clojure.test :refer :all]
            [juxt.iota :refer [given]]
            [galapagos.core :as core]
            [galapagos.schema :as schema]
            [galapagos.schema.blog :refer [QueryRoot]]))

(def blog-schema
  (schema/create-schema QueryRoot))

(deftest core-test
  (testing "Simple query of existing post"
    (given (core/execute!! blog-schema "{ post(id: 1) { title } }")
      :post := {:title "Post #1"})
    (given (core/execute!! blog-schema "{ post(id: 1) { id, title } }")
      :post := {:title "Post #1" :id 1}))

  (testing "Non-existent post"
    (given (core/execute!! blog-schema "{ post(id: 99) { title } }")
      :post := {}))

  (testing "Nested queries"
    (given (core/execute!! blog-schema "{ post(id: 1) { author { name } } }")
      :post := {:author {:name "Author Of Post #1"}})
    (given (core/execute!! blog-schema "{ post(id: 1) { author { name, preferredEditor } } }")
      :post := {:author {:name "Author Of Post #1" :preferredEditor :vim}}))

  (testing "Named fields"
    (given (core/execute!! blog-schema
             "query readPost { firstPost: post(id: 1) { title } }")
      :firstPost := {:title "Post #1"}))

  (testing "Lists"
    (given (core/execute!! blog-schema "{ posts { id, title } }")
      :posts := [{:id 1 :title "Some post"}
                 {:id 2 :title "Another post"}])
    (given (core/execute!! blog-schema "{ posts { id, title, author { name } } }")
      :posts := [{:id 1 :title "Some post" :author {:name "Author Of Some post"}}
                 {:id 2 :title "Another post" :author {:name "Author Of Another post"}}])
    (given (core/execute!! blog-schema "{ posts { author { name } } }")
      :posts := [{:author {:name "Author Of Some post"}}
                 {:author {:name "Author Of Another post"}}])))



