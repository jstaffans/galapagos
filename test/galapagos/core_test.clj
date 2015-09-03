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
      :data := {:post {:title "Post #1"}})
    (given (core/execute!! blog-schema "{ post(id: 1) { id, title } }")
      :data := {:post {:title "Post #1" :id 1}}))

  (testing "Non-existent post"
    (given (core/execute!! blog-schema "{ post(id: 99) { title } }")
      :data := {:post {}}))

  (testing "Nested queries"
    (given (core/execute!! blog-schema "{ post(id: 1) { author { name } } }")
      :data := {:post {:author {:name "Author Of Post #1"}}})
    (given (core/execute!! blog-schema "{ post(id: 1) { author { name, preferredEditor } } }")
      :data := {:post {:author {:name "Author Of Post #1" :preferredEditor :vim}}}))

  (testing "Named fields"
    (given (core/execute!! blog-schema
             "{ firstPost: post(id: 1) { title } }")
      :data := {:firstPost {:title "Post #1"}}))

  (testing "Siblings"
    (given (core/execute!! blog-schema
             "{ firstPost: post(id: 1) { title } secondPost: post(id: 2) { title } }")
      :data := {:firstPost  {:title "Post #1"}
                :secondPost {:title "Post #2"}}))

  (testing "Nodes that solve to primitives"
    (given (core/execute!! blog-schema
             "{ post(id: 1) { author { profilePicture } } }")
      :data := {:post {:author {:profilePicture "url/for/id/123?size=default"}}})
    (given (core/execute!! blog-schema
             "{ post(id: 1) { author { profilePicture(size: small) } } }")
      :data := {:post {:author {:profilePicture "url/for/id/123?size=small"}}}))

  (testing "Lists"
    (given (core/execute!! blog-schema "{ posts { id, title } }")
      :data := {:posts [{:id 1 :title "Some post"}
                        {:id 2 :title "Another post"}]})
    (given (core/execute!! blog-schema "{ posts { id, title, author { name } } }")
      :data := {:posts [{:id 1 :title "Some post" :author {:name "Author Of Some post"}}
                        {:id 2 :title "Another post" :author {:name "Author Of Another post"}}]})
    (given (core/execute!! blog-schema "{ posts { author { name } } }")
      :data := {:posts [{:author {:name "Author Of Some post"}}
                        {:author {:name "Author Of Another post"}}]})))



