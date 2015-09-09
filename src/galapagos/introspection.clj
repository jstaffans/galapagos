(ns galapagos.introspection
  (:require [galapagos.util :as util]))

(defn- register-type
  [types type]
  (let [name (-> (meta type) :introspection :name)]
    (swap! types #(assoc % name (assoc type :name name)))))

(defn walk
  [root types]
  (clojure.walk/postwalk
    #(cond
      (-> (meta %) :introspection :is-type?) (register-type types %)
      :else %)
    root)
  types)