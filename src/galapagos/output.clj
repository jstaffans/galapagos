(ns galapagos.output
  (:require [clojure.walk :as walk])
  (:refer-clojure :exclude [spit]))

(defn collect-fields [result]
  (let [fields (:fields result)]
    (into {} (walk/postwalk
               #(if-let [solution (:solution  %)]
                 (if (empty? (:fields %)) solution (collect-fields %))
                 %)
               fields))))

(defn collect
  "Collects a solved context into a simple map."
  [context]
  (let [root-key (first (keys context))]
    {root-key (collect-fields (get context root-key))})  )


