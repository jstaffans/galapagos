(ns galapagos.schema.helpers)

;:fields      [:id     schema/GraphQLInt!     "The ID"
;              :title  schema/GraphQLString!  "The title"
;              :date   PublishingDate
;              :author 'galapagos.schema.blog/FindAuthor]


(defn to-field-definition
  [vec]
  (let [field-def (into {} (map (fn [k v] (assoc {} k v)) [:type :description] vec))
        type (:type field-def)]
    (merge field-def (if (= (.length type) (inc (.lastIndexOf type "!")))
                       {:type (apply str (butlast type)) :required true}
                       {:required false}))
    ))

(defn to-field-map
  "Converts a vector of field declarations as per the schema definition
  DSL to a map, which is easier to process internally. Example:
  [:id schema/GraphQLInt! \"The id\"] ->
  {:id {:type schema/GraphQLInt
        :description \"The id\"
        :required true}}"
  [v]
  (let [parts (partition-by keyword? v)
        keys (->> (take-nth 2 parts) (map (partial apply keyword)))
        defs (take-nth 2 (rest parts))
        def-maps (map to-field-definition defs)
        fields (into {} (map (fn [k f] (assoc {} k f)) keys def-maps))]
    fields))
