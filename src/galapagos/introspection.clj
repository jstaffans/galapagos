(ns galapagos.introspection)

;; ## Introspection
;;
;; Talk about being meta! GraphQL can answer queries about itself, ie about its schema.
;; This is defined precisely in https://facebook.github.io/graphql/#sec-Schema-Introspection .
;; This namespace contains functions that build an overview map of the types used in the schema.


(defn- register-type!
  "Stateful registration of a type. Besides normal information such as the description
  and the field map, we also register introspection metadata such as the *kind* of type."
  [types & {:keys [type metadata]}]
  (let [[type-name desc kind] ((juxt :name :description :kind) metadata)]
    (swap! types #(assoc % type-name (assoc type :__name type-name :description desc :__kind kind)))))


(defn- walk-fields
  "Recursively walks the fields of a type and registers any types found. It may happen
  that a type is registered multiple times, the previous registration then being overwritten.
  This only happens at startup though, not when an introspection query is performed,
  so the performance hit is negligible."
  [node types]
  (doseq [[_ field] (:fields node)]
    (let [type (cond
                 (map? (:type field)) (:type field)
                 :else {})]
      (register-type! types
        :type type
        :metadata (:introspection (or (meta field) (meta type))))))
  (doseq [[_ {:keys [type]}] (:fields node)]
    (walk-fields type types)))


(defn- walk
  "Registers interface types and then starts recursion of the fields of the root query type."
  [root types]
  ;; Register interfaces using the top-level map
  (doseq [interface (vals (:interfaces root))]
    (register-type! types
      :metadata (:introspection (meta interface))))
  (walk-fields root types)
  types)


(defn type-map
  "Returns a map of all types used in the given schema."
  [root]
  @(walk root (atom {})))


