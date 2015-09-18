(ns galapagos.introspection-test
  (:require [clojure.test :refer :all]
            [juxt.iota :refer [given]]
            [galapagos.introspection :as introspection]
            [galapagos.schema.blog :refer [QueryRoot]]))


(deftest introspection-test

  (testing "Type kinds"
    (given (introspection/type-map QueryRoot)
      :BlogUser         :⊃ {:__kind :INTERFACE}
      :Blogger          :⊃ {:__kind :UNION}
      :Post             :⊃ {:__kind :OBJECT}
      :Author           :⊃ {:__kind :OBJECT}
      :PublishingDate   :⊃ {:__kind :SCALAR}
      :PreferredEditor  :⊃ {:__kind :ENUM}

      ;; Builtin types
      :String           :⊃ {:__kind :SCALAR}
      :Int              :⊃ {:__kind :SCALAR})))
