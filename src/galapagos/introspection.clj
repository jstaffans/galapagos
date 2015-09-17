(ns galapagos.introspection)

(defn- register-type
  [types type]
  (let [introspection-metadata (-> (meta type) :introspection)
        name (:name introspection-metadata)
        kind (:kind introspection-metadata)]
    (swap! types #(assoc % name (assoc type :__name name :__kind kind)))))

(defn walk
  [root types]
  (clojure.walk/postwalk
    #(cond
      (contains? (meta %) :introspection) (register-type types %)
      :else %)
    root)
  types)