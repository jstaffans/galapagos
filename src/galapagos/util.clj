(ns galapagos.util
  (:import (schema.core EnumSchema Predicate)))

;; TODO: pretty crude way of determining if something is a scalar
(defn scalar?
  [t]
  (or
    (= Class (type t))
    (= Predicate (type t))
    (= EnumSchema (type t))))

(defn apply-1
  "Applies f to val, or shallowly to each element of val if val implements Sequential."
 [f val]
  (if (sequential? val)
    (map f val)
    (f val)))
