(ns graphql-clj.validator.rules.no-undefined-variables
  "A GraphQL operation is only valid if all variables encountered, both directly
   and via fragment spreads, are defined by that operation."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]
            [clojure.spec :as s]
            [graphql-clj.spec :as spec]
            [graphql-clj.box :as box]))

(defn- undefined-variable-error [{:keys [variable-name]}]
  (format "Variable '$%s' is not defined." (box/box->val variable-name)))

(defnodevisitor undefined-variable :pre :argument
  [{:keys [variable-name] :as n} s]
  (when variable-name                                       ;; TODO we can use the variable-usages for this
    (if-not (s/get-spec (spec/spec-for {:node-type :variable-usage :variable-name (box/box->val variable-name)} s))
      {:state (ve/update-errors s (undefined-variable-error n))
       :node (update n :variable-name box/box->val)}
      {:node (update n :variable-name box/box->val)})))

(def rules [undefined-variable])
