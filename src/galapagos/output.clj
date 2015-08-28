(ns galapagos.output
  (:require [clojure.walk :as walk])
  (:refer-clojure :exclude [spit]))

(defn- many-results?
  [result]
  (vector? (:solution result)))

(defn- leaf?
  [field]
  (empty? (:fields field)))

(defn- collect-fields [result]
  (let [fields (if (many-results? result)
                 (map (partial into {}) (:fields result))
                 (into {} (:fields result)))]
    (walk/postwalk
      (fn [field]
        (if-let [solution (:solution field)]
          (if (leaf? field)
            solution
            (collect-fields field))
          field))
      fields)))

(defn collect
  "Collects a solved context into a simple map."
  [context]
  (let [root-key (first (keys context))]
    {root-key (collect-fields (get context root-key))}))


