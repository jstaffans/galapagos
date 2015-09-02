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
      :post := {:title "Some post"})
    (given (core/execute!! blog-schema "{ post(id: 1) { id, title } }")
      :post := {:title "Some post" :id 1}))

  (testing "Non-existent post"
    (given (core/execute!! blog-schema "{ post(id: 99) { title } }")
      :post := {}))

  (testing "Nested queries"
    (given (core/execute!! blog-schema "{ post(id: 1) { author { name } } }")
      :post := {:author {:name "Author Of Some post"}})
    (given (core/execute!! blog-schema "{ post(id: 1) { author { name, preferredEditor } } }")
      :post := {:author {:name "Author Of Some post" :preferredEditor :vim}}))

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



