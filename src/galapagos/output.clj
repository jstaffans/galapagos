(ns galapagos.output
  (:require [clojure.walk :as walk])
  (:refer-clojure :exclude [spit]))

;; This namespace is no longer needed because muse handles resolution all the way down.
;; Could be useful for introspection though ..

(defn- many-results?
  [result]
  (vector? (:solution result)))

(defn- leaf?
  [field]
  (empty? (:fields field)))

(defn- collect-fields [result]
  (let [fields (if (many-results? result)
                 (map (partial into {}) (:fields result))
                 (into {} (:fields result)))
        keep-solutions (fn [field]
                         (if-let [solution (:solution field)]
                           (if (leaf? field) solution (collect-fields field))
                           field))]
    (walk/postwalk keep-solutions fields)))

(defn collect
  "Collects a solved context into a simple map."
  [context]
  (let [root-key (first (keys context))]
    {root-key (collect-fields (get context root-key))}))


