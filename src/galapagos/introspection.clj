(ns galapagos.introspection)

(defn- register-type
  [types type introspection-metadata]
  (let [name (:name introspection-metadata)
        kind (:kind introspection-metadata)]
    (swap! types #(assoc % name (assoc type :__name name :__kind kind)))))


(defn walk
  [root types]
  ;; Register interfaces using the top-level map
  (doseq [interface (vals (:interfaces root))]
    (register-type types interface (:introspection (meta interface))))

  ;; Dumbly walk through the schema and register all types. Doing it this way
  ;; means that any type that isn't used anywhere in the schema, but still
  ;; defined with `galapagos.schema/deftype` or another macro,; will not show
  ;; up during introspection.
  (clojure.walk/postwalk
    #(if-let [fields (:fields %)]
      (doseq [f (vals fields)]
        (if-let [metadata (or (meta f) (meta (:type f)))]
          (register-type types f (:introspection metadata))
          %))
      %) root)
  types)