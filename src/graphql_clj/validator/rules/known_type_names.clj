(ns graphql-clj.validator.rules.known-type-names
  "A GraphQL document is only valid if referenced types are defined by the type schema."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]
            [clojure.spec :as s]))

(defn- unknown-type-error [{:keys [type-name]} s]
  (format "Unknown type '%s'." type-name))

(defn- unknown-type-name* [{:keys [spec] :as n} s]
  (when-not (s/get-spec spec)
    {:state (ve/update-errors s (unknown-type-error n s))
     :break true}))

(defnodevisitor unknown-type-name :pre :variable-definition [n s]
  (unknown-type-name* n s))

(def rules [unknown-type-name])
