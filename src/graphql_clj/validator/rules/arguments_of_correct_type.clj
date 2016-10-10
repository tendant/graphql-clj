(ns graphql-clj.validator.rules.arguments-of-correct-type
  "A GraphQL document is only valid if all field argument literal values are of the type expected by their position."
  (:require [graphql-clj.validator.errors :as e]
            [graphql-clj.visitor :refer [defmapvisitor]]
            [clojure.spec :as s]))

(defn- bad-value-error [arg-name type value]
  (format "Argument '$%s' of type '%s' has invalid value: %s. Reason: %s value expected."
          arg-name type value type))

(defmapvisitor bad-value :post [{:keys [spec variable-name value type-name node-type]} s]
  (case node-type
    :argument
    (when (and spec value (not (s/valid? spec value)))
      {:state (e/update-errors s (bad-value-error variable-name type-name value))})
    nil))

(def rules [bad-value])
