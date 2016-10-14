(ns graphql-clj.validator.rules.known-fragment-names
  "A GraphQL document is only valid if all `...Fragment` fragment spreads refer to fragments defined in the same document."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]
            [clojure.spec :as s]))

(defn- unknown-fragment-error [{:keys [name]}]
  (format "Unknown fragment '%s'." name))

(defnodevisitor unknown-fragment-name :pre :fragment-spread
  [{:keys [spec] :as n} s]
  (when-not (s/get-spec spec)
    {:state (ve/update-errors s (unknown-fragment-error n))
     :break true}))

(def rules [unknown-fragment-name])
