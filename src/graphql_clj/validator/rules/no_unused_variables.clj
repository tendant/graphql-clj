(ns graphql-clj.validator.rules.no-unused-variables
  "A GraphQL operation is only valid if all variables defined by an operation
   are used, either directly or within a spread fragment."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]
            [clojure.set :as set]))

(defn- unused-variable-error [{:keys [operation-type]} variable-name]
  (if (:name operation-type)
    (format "Variable '$%s' is never used in operation '%s'." variable-name (:name operation-type))
    (format "Variable '$%s' is never used." variable-name)))

(defnodevisitor variable-def :pre :variable-definition
  [{:keys [variable-name]} s]
  (when variable-name
    {:state (update s :var-defs conj variable-name)}))

(defnodevisitor variable-use :pre :argument
  [{:keys [variable-name] :as n} s]
  (when variable-name
    {:state (update s :var-usages conj variable-name)}))

(defnodevisitor unused-variables :post :operation-definition
  [n {:keys [var-defs var-usages] :as s}]
  (let [delta (set/difference (set var-defs) (set var-usages))]
    (when-not (empty? delta)
      {:state (apply ve/update-errors s (map #(unused-variable-error n %) delta))})))

(def rules [variable-def
            variable-use
            unused-variables])
