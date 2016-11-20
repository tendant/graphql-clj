(ns graphql-clj.validator.rules.fields-on-correct-type
  "A GraphQL document is only valid if all fields selected are defined by the
   parent type, or are an allowed meta field such as __typename."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]
            [clojure.spec :as s]))

(defn- missing-type-error [{:keys [spec parent-type-name base-name] :as n}]
  {:error (format "Cannot query field '%s' on type '%s'." (name spec) (or parent-type-name base-name))
   :loc   (ve/extract-loc (meta n))})

;; TODO allowed meta field such as __typename?

(defnodevisitor missing-type :pre :field
  [{:keys [spec v/path] :as n} s]
  (when-not (s/get-spec spec)
    {:state (ve/update-errors s (missing-type-error n))
     :break true}))

(def rules [missing-type])
