(ns galapagos.output
  (:require [clojure.walk :as walk])
  (:refer-clojure :exclude [spit]))

;; TODO: do nested contexts work?

(defn collect-fields [result]
  (let [fields (:fields result)]
    (into {} (walk/postwalk
               #(if
                 (and (:solution %) (empty? (:fields %)))
                 (:solution %)
                 %)
               fields))))

(defn collect
  "Collects a solved context into a simple map."
  [context]
  (let [root-key (first (keys context))]
    {root-key (collect-fields (get context root-key))})  )


