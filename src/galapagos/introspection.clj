(ns galapagos.introspection)

(defn register-type
  [types type]
  (let [name (-> (meta type) :introspection :name)]
    (swap! types #(assoc % name (assoc type :name name)))))

(defn walk
  [root types]
  (clojure.walk/postwalk
    #(if (-> (meta %) :introspection :is-type?)
      (register-type types %)
      %) root)
  types)