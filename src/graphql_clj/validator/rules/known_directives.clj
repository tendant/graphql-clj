(ns graphql-clj.validator.rules.known-directives
  "A GraphQL document is only valid if all `@directives` are known by the
   schema and legally positioned."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]
            [graphql-clj.spec :as spec]))

(defn- undefined-directive-error [{:keys [name]}]
  (format "Unknown directive '@%s'." name))

(defnodevisitor undefined-directive :pre :directive
  [{:keys [name] :as n} s]
  (when-not (get spec/directive-specs name)
    {:state (ve/update-errors s (undefined-directive-error n))
     :break true}))

(def rules [undefined-directive])
