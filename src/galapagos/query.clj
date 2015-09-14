(ns galapagos.query
  (:require [instaparse.core :as insta])
  (:import [IllegalArgumentException]))

;; ## Query parsing
;;
;; This namespace provides GraphQL query parsing facilities.
;; It uses an Instaparse grammar adapted from Huey Petersen's
;; HueyQL prototypeä (https://github.com/eyston/hueyql).
;;
;; ### Validation
;;
;; Not much is done as far as validation goes at the moment.
;; If a query can't be parsed at all using the defined grammar,
;; an `IllegalArgumentException` is thrown.

(def parser
  (insta/parser "ROOT = INTRO? SELECTION <whitespace> FRAGMENTS? <whitespace>
                 INTRO = OP <whitespace> NAME <whitespace> (VARS)?
                 OP = 'query' | 'mutation'
                 NAME = token
                 VARS = <whitespace> <'('> VAR (<','> VAR)* <')'> <whitespace>
                 <VAR> = <whitespace> VAR_NAME <':'> <whitespace> VAR_TYPE <whitespace>
                 VAR_NAME = <'$'> token
                 VAR_TYPE = ('String' | 'Int' | 'Float' | 'Boolean' | 'ID')
                 ARGS = <whitespace> <'('> ARG (<','> ARG)* <')'> <whitespace>
                 <ARG> = <whitespace> ARG_NAME <':'> <whitespace> (ARG_VALUE_LITERAL | ARG_VALUE_LIST) <whitespace>
                 ARG_NAME = token
                 ARG_VALUE_LITERAL = #'[^\\[,) ]+'
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

(defn- is-field-selection?
  "Checks if a particular selection is a field, as opposed to a
  fragment or fragment reference."
  [field]
  (and (coll? field) (contains? field :name)))

(defn- parse-literal
  "Some literals need an extra parsing step, such as single-quoted strings."
  [literal]
  (if-let [quotes-stripped (last (re-matches #"^[\"'](.*)[\"']$" literal))]
    quotes-stripped
    literal))

;; Besides the Instaparse grammar, this is the heart of the query
;; parsing. It turns the tree structure returned by Instaparse to
;; something that the galapagos execution engine understands.
(def transform
  (partial insta/transform {:INTRO             (fn [& args] (into {} args))
                            :OP                (fn [op] [:op (keyword op)])
                            :NAME              (fn [name] [:name (keyword name)])
                            :VARS              (fn [& vars] [:variables (zipmap (take-nth 2 vars) (take-nth 2 (rest vars)))])
                            :VAR_NAME          (fn [name] (str "$" name))
                            :VAR_TYPE          (fn [type] type)
                            :ARGS              (fn [& args] [:args (zipmap (take-nth 2 args) (take-nth 2 (rest args)))])
                            :ARG_NAME          (fn [name] (keyword name))
                            :ARG_VALUE_LITERAL (fn [literal] (parse-literal literal))
                            :ARG_VALUE_LIST    (fn [& list] (->> (clojure.string/split (apply str list) #"[,][ ]*")
                                                                 (mapv parse-literal)))
                            :ALIAS             (fn [alias] [:alias (keyword alias)])
                            :SELECTION         (fn [& selections]
                                                 (let [selection-set (into #{} selections)
                                                       field-set (into #{} (filter is-field-selection? selection-set))
                                                       fragment-references-set (clojure.set/difference selection-set field-set)]
                                                   (assoc {}
                                                     :fields (vec field-set)
                                                     :fragments (vec fragment-references-set))))
                            :INLINE_FRAGMENT   (fn [& inline-fragment] (into {} inline-fragment))
                            :FRAGMENT_SPREAD   (fn [fragment-spread] fragment-spread)
                            :FRAGMENT_NAME     (fn [fragment] (keyword fragment))
                            :FIELDS            (fn [& fields] [:fields (vec fields)])
                            :FIELD             (fn [& field] (into {} field))
                            :FRAGMENTS         (fn [& fragments] [:fragments (into {} fragments)])
                            :FRAGMENT          (fn [fragment-name & args] {fragment-name (into {} args)})
                            :FRAGMENT_ON       (fn [on] [:on (keyword on)])
                            :ROOT              (fn [& args]
                                                 (merge (into {} args) {:op :query}))}))

(defn parse [string]
  "The main parsing entry point."
  (let [tree (parser string)]
    (if (insta/failure? tree)
      (do
        (print (insta/get-failure tree))
        (throw (IllegalArgumentException. "Could not parse query!")))
      (transform tree))))

