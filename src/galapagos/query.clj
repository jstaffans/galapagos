(ns galapagos.query
  (:require [instaparse.core :as insta])
  (:import [IllegalArgumentException]))

;; Credits go to Huey Petersen (https://github.com/eyston/hueyql), I adapted this from his parser

(def parser
  (insta/parser "ROOT = INTRO? SELECTION <whitespace> FRAGMENTS? <whitespace>
                 INTRO = OP <whitespace> NAME
                 OP = 'query' | 'mutation'
                 NAME = token
                 ARGS = <whitespace> <'('> ARG (<','> ARG)* <')'> <whitespace>
                 <ARG> = <whitespace> ARG_NAME <':'> <whitespace> (ARG_VALUE_SCALAR | ARG_VALUE_LIST) <whitespace>
                 ARG_NAME = token
                 ARG_VALUE_SCALAR = #'[^\\[,) ]+'
                 ARG_VALUE_LIST = <'['> <whitespace> (#'.'+) <whitespace> <']'>
                 SELECTION = <whitespace> <'{'> (FIELD | FRAGMENT_SPREAD) ((<','>)? (FIELD | FRAGMENT_SPREAD))* <'}'> <whitespace>
                 FRAGMENT_SPREAD = <whitespace> <'...'> (<whitespace>)? (FRAGMENT_NAME | INLINE_FRAGMENT) <whitespace>
                 INLINE_FRAGMENT = <'on'> <whitespace> FRAGMENT_ON (SELECTION)*
                 FIELD = <whitespace> (ALIAS <':'> <whitespace>)? NAME(ARGS)? <whitespace> SELECTION? <whitespace>
                 ALIAS = token
                 FRAGMENTS = FRAGMENT*
                 FRAGMENT = <'fragment'> <whitespace> FRAGMENT_NAME <whitespace> <'on'> <whitespace> FRAGMENT_ON (SELECTION)*
                 FRAGMENT_NAME = token
                 FRAGMENT_ON = token
                 <token> = #'\\w+'
                 whitespace = #'\\s*'"))

(defn- trim-quotes
  "Trims single and double quotes from start and end of string."
  [s]
  (clojure.string/replace s #"(^[\"']|[\"']$)" ""))

(defn- is-field-selection?
  [field]
  (and (coll? field) (contains? field :name)))

(def transform
  (partial insta/transform {:INTRO            (fn [& args] (into {} args))
                            :OP               (fn [op] [:op (keyword op)])
                            :NAME             (fn [name] [:name (keyword name)])
                            :ARGS             (fn [& args] [:args (zipmap (take-nth 2 args) (take-nth 2 (rest args)))])
                            :ARG_NAME         (fn [name] (keyword name))
                            :ARG_VALUE_SCALAR (fn [value] value)
                            :ARG_VALUE_LIST   (fn [& list] (->> (clojure.string/split (apply str list) #"[,][ ]*")
                                                                (mapv trim-quotes)))
                            :ALIAS            (fn [alias] [:alias (keyword alias)])
                            :SELECTION        (fn [& selections]
                                                (let [selection-set (into #{} selections)
                                                      field-set (into #{} (filter is-field-selection? selection-set))
                                                      fragment-references-set (clojure.set/difference selection-set field-set)]
                                                  (assoc {}
                                                    :fields (vec field-set)
                                                    :fragments (vec fragment-references-set))))
                            :INLINE_FRAGMENT  (fn [& inline-fragment] (into {} inline-fragment))
                            :FRAGMENT_SPREAD  (fn [fragment-spread] fragment-spread)
                            :FRAGMENT_NAME    (fn [fragment] (keyword fragment))
                            :FIELDS           (fn [& fields] [:fields (vec fields)])
                            :FIELD            (fn [& field] (into {} field))
                            :FRAGMENTS        (fn [& fragments] [:fragments (into {} fragments)])
                            :FRAGMENT         (fn [fragment-name & args] {fragment-name (into {} args)})
                            :FRAGMENT_ON      (fn [on] [:on (keyword on)])
                            :ROOT             (fn [& args]
                                                (merge (into {} args) {:op :query}))}))

(defn parse [string]
  (let [tree (parser string)]
    (if (insta/failure? tree)
      (do
        (print (insta/get-failure tree))
        (throw (IllegalArgumentException. "Could not parse query!")))
      (transform tree))))

