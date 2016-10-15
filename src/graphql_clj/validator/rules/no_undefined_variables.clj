(ns graphql-clj.validator.rules.no-undefined-variables
  "A GraphQL operation is only valid if all variables encountered, both directly
   and via fragment spreads, are defined by that operation."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]
            [clojure.spec :as s]
            [graphql-clj.spec :as spec]))

(defn- undefined-variable-error [{:keys [variable-name]}]
  (format "Variable '$%s' is not defined." variable-name))

(defnodevisitor undefined-variable :pre :argument
  [{:keys [variable-name] :as n} s]
  (when variable-name
    (when-not (s/get-spec (spec/spec-for s {:node-type :variable-usage :variable-name variable-name}))
      {:state (ve/update-errors s (undefined-variable-error n))})))

(def rules [undefined-variable])
