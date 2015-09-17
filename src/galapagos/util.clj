(ns galapagos.util)

;; ## Util
;;
;; Utility functions.


(defn apply-1
  "Applies f to val, or shallowly to each element of val if val implements Sequential."
 [f val]
  (if (sequential? val)
    (map f val)
    (f val)))
