(ns galapagos.schema
  (:require [galapagos.introspection :as introspection]
            [schema.core :as s]
            [clojure.core.async :as async])
  (:refer-clojure :exclude [deftype definterface]))

(defn scalar?
  "Check if a a node's type is scalar or enum. Relies on introspection metadata."
  [node]
  (contains? #{:ENUM :SCALAR} (-> (meta node) :introspection :kind)))

(defn- add-introspection-info
  "Adds introspection information to individual fields. Assumes that each field
  var has an `:introspection` key in **its** metadata."
  [t]
  (reduce-kv (fn [acc k v]
               (assoc acc k (with-meta v {:introspection (-> (:type v) symbol resolve meta :introspection)})))
    {} (:fields t)))

(defmacro defscalar
  [name kind]
  `(def
     ~(vary-meta name assoc :introspection {:name (keyword name) :kind :SCALAR})
     ~kind))

;; ID can be an integer or an UUID
(defscalar GraphQLID (s/if (partial re-matches #"^\d+$") s/Int s/Uuid))

(defscalar GraphQLInt s/Int)

(defscalar GraphQLFloat s/Num)

(defscalar GraphQLString s/Str)

(defscalar GraphQLBoolean s/Bool)

(defmacro defenum
  [name & values]
  `(def
     ~(vary-meta name assoc :introspection {:name (keyword name) :kind :ENUM})
     (s/enum ~@values)))

(defmacro definterface
  [name t]
  (let [fields-with-type-names (add-introspection-info t)]
    `(def ~name (merge ~t {:fields ~fields-with-type-names}))))

(defmacro defunion
  [name ts]
  `(def ~(vary-meta name assoc :introspection {:name (keyword name) :kind :UNION})
     {:fields     (into {} (map :fields ~ts))
      :interfaces (mapcat :interfaces ~ts)}))

(defmacro deftype
  "Define a type corresponding to the GraphQL object type."
  [name interfaces t]
  (let [interface-names (into [] (map str interfaces))
        fields-with-type-names (add-introspection-info t)]
    `(do
       (def ~(vary-meta name assoc :introspection {:name (keyword name) :kind :OBJECT})
         (merge ~t {:interfaces (map keyword ~interface-names)} {:fields ~fields-with-type-names}))
       (defn ~(symbol (str '-> name)) [v#] (with-meta v# {:type ~(keyword name)})))))

(defmacro deffield
  "Defines a field that fetches something."
  [name s ret f]
  (if (= :- s)
    (let [[type arity] (if (vector? ret) [(first ret) :many] [ret :one])]
      `(def ~(vary-meta name assoc
               :introspection
               {:name (keyword name)
                :kind (-> type symbol resolve meta :introspection :kind)})
         (merge
           (assoc ~f :fields (:fields ~type) :type '~type :type-definition ~type :arity ~arity)
           {:returns ~ret})))
    (throw (IllegalArgumentException. (str "Unknown schema definition operator: " s)))))

;; Introspection types

(defenum TypeKind :SCALAR :OBJECT :INTERFACE :UNION :ENUM :INPUT_OBJECT :NON_NULL)

(deftype TypeDescription []
  {:fields {:kind        {:type TypeKind}
            :name        {:type GraphQLString}
            :description {:type GraphQLString}}})


(deffield FindType :- TypeDescription
  {:description "Finds a type by name"
   :args        {:name GraphQLString}
   ; solve added once we have the root
   })

(defn- solve-type
  [type-map]
  (fn [{:keys [name]}]
    (async/go
      (let [type-definition (get type-map (keyword name))]
        (->TypeDescription
          {:name        (:name type-definition)
           :description (:description type-definition)})))))

(defmacro defroot
  [name r]
  `(def ~name ~r))

(defn- build-type-map
  [root]
  (let [types (introspection/walk root (atom {}))]
    (reduce-kv (fn [acc name type] (assoc acc name type)) {} @types)))

;; TODO: can perform any pre-processing here
(defn create-schema
  [root]
  (let [type-map (build-type-map root)]
    {:root (assoc-in root [:fields :__type :type] (assoc FindType :solve (solve-type type-map)))}))

