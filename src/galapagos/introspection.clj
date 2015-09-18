(ns galapagos.introspection)

(defn- register-type
  [types & {:keys [metadata]}]
  (let [name (:name metadata)
        desc (:description metadata)
        kind (:kind metadata)]
    (swap! types #(assoc % name (assoc {} :description desc :__name name :__kind kind)))))


(defn walk
  [root types]
  ;; Register interfaces using the top-level map
  (doseq [interface (vals (:interfaces root))]
    (register-type types
      :metadata (:introspection (meta interface))))

  ;; Dumbly walk through the schema and register all types. Doing it this way
  ;; means that any type that isn't used anywhere in the schema, but still
  ;; defined with `galapagos.schema/deftype` or another macro,; will not show
  ;; up during introspection.
  (clojure.walk/postwalk
    #(if-let [fields (:fields %)]
      (doseq [f (vals fields)]
        (if-let [metadata (or (meta f) (meta (:type f)))]
          (do
            (when (= :Post (-> metadata :introspection :name)) (println %))
            (register-type types
              :metadata (:introspection metadata)))
          %))
      %) root)
  types)