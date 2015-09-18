(ns galapagos.introspection)

(defn- register-type
  [types & {:keys [metadata]}]
  (let [[type-name desc kind] ((juxt :name :description :kind) metadata)]
    (swap! types #(assoc % type-name (assoc {} :description desc :__name type-name :__kind kind)))))

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
          (register-type types
            :metadata (:introspection metadata))
          %))
      %) root)
  types)