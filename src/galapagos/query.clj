(ns galapagos.query
  (:require [instaparse.core :as insta])
  (:import [IllegalArgumentException]))

;; Credits go to Huey Petersen (https://github.com/eyston/hueyql), I adapted this from his parser

(def parser
  (insta/parser "ROOT = INTRO? IDS? FIELDS <whitespace>
                 INTRO = OP <whitespace> NAME
                 OP = 'query' | 'mutation'
                 NAME = token
                 IDS = <whitespace> <'('> ARG (<','> ARG)* <')'> <whitespace>
                 ARGS = <whitespace> <'('> ARG (<','> ARG)* <')'> <whitespace>
                 <ARG> = <whitespace> #'[^,)]+' <whitespace>
                 FIELDS = <whitespace> <'{'> FIELD ((<','>)? FIELD)* <'}'> <whitespace>
                 FIELD = <whitespace> (ALIAS <':'> <whitespace>)? NAME(ARGS | CALLS)? <whitespace> FIELDS? <whitespace>
                 ALIAS = token
                 CALLS = CALL+
                 CALL = <'.'> NAME ARGS
                 <token> = #'\\w+'
                 whitespace = #'\\s*'"))


(defn map-from-args
  "Converts an argument string to a map:
   \"id: 1\" -> {:id \"1\"}"
  [args]
  (let [split-args (clojure.string/split args #",\s*")
        pairs (map #(clojure.string/split % #":\s*") split-args)]
  (reduce #(assoc %1 (keyword (first %2)) (second %2)) {} pairs)))


(def transform
  (partial insta/transform {:INTRO  (fn [& args] (into {} args))
                            :OP     (fn [op] [:op (keyword op)])
                            :NAME   (fn [name] [:name (keyword name)])
                            :IDS    (fn [& args] [:ids (vec args)])
                            :ARGS   (fn [& args] [:args (into {} (map map-from-args args))])
                            :ALIAS  (fn [alias] [:alias (keyword alias)])
                            :CALLS  (fn [& calls] [:calls (vec calls)])
                            :CALL   (fn [name args] (into {} [name args]))
                            :FIELDS (fn [& fields] [:fields (vec fields)])
                            :FIELD  (fn [& args] (into {} args))
                            :ROOT   (fn [& args]
                                      (merge (into {} args) {:op :query}))}))

(defn parse [string]
  (let [tree (parser string)]
    (if (insta/failure? tree)
      (do
        (print (insta/get-failure tree))
        (throw (IllegalArgumentException. "Could not parse query!")))
      (transform tree))))

