(ns galapagos.util)

(defn scalar?
  "Check if a a node's type is scalar or enum. Relies on introspection metadata."
  [node]
  (contains? #{:ENUM :SCALAR} (-> (meta node) :introspection :kind)))

(defn apply-1
  "Applies f to val, or shallowly to each element of val if val implements Sequential."
 [f val]
  (if (sequential? val)
    (map f val)
    (f val)))
