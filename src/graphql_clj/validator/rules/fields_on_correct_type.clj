(ns graphql-clj.validator.rules.fields-on-correct-type
  "A GraphQL document is only valid if all fields selected are defined by the
   parent type, or are an allowed meta field such as __typename."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]
            [clojure.spec :as s]
            [graphql-clj.spec :as spec]))

(defn- missing-type-error [{:keys [spec] :as n} s]
  {:error (format "Cannot query field '%s' on type '%s'." (name spec) (name (spec/get-parent-type n s)))
   :loc   (ve/extract-loc (meta n))})

;; TODO allowed meta field such as __typename?

(defnodevisitor missing-type :pre :field
  [{:keys [spec v/path] :as n} s]
  (when-not (s/get-spec spec)
    {:state (ve/update-errors s (missing-type-error n s))
     :break true}))

(def rules [missing-type])
