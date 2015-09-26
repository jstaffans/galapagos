(ns galapagos.schema.helpers
  (:require [medley.core :refer [map-vals]]
            [schema.core :as s]))

(defn- to-field-definition
  [v]
  (let [required? (keyword? (some #{:!} v))
        field-def (into {} (map #(assoc {} %1 %2)
                             [:type :description]
                             (if required? (filter #(not= :! %) v) v)))
        t (:type field-def)]
    (if (or (vector? t) (symbol? t))
      (merge field-def {:required? required?})
      (merge field-def {:required? required? :var t}))))

(defn to-field-map
  "Converts a vector of field declarations as per the schema definition
  DSL to a map, which is easier to process internally. Example:
  [:id schema/GraphQLInt :! \"The id\"] ->
  {:id {:type schema/GraphQLInt
        :description \"The id\"
        :required true}}"
  [v]
  (let [parts (partition-by #(and (keyword? %) (not= :! %)) v)
        keys (->> (take-nth 2 parts) (map (partial apply keyword)))
        defs (take-nth 2 (rest parts))
        def-maps (map to-field-definition defs)
        fields (into {} (map (fn [k f] (assoc {} k f)) keys def-maps))]
    fields))

(defn- coercion
  [arg arg-def]
  (assoc {} (if (:required? arg-def) arg (s/optional-key arg)) (:type arg-def)))

(defn to-arg-map
  "Similarily to `to-field-map`, converts a vector of input value declarations
  into a map. Adds a coercion entry for input argument coercion purposes."
  [v]
  (let [field-map (to-field-map v)]
    (into {} (map (fn [[arg arg-def]]
                    (assoc {} arg (merge arg-def {:coercion (coercion arg arg-def)})))
               field-map))))