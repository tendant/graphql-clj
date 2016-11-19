(ns graphql-clj.validator.rules.fields-on-correct-type
  "A GraphQL document is only valid if all fields selected are defined by the
   parent type, or are an allowed meta field such as __typename."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]
            [clojure.spec :as s]
            [graphql-clj.spec :as spec]))

(defn- type-label [{:keys [spec] :as n} s]
  (name (or (s/get-spec spec) (let [p (spec/get-parent-type n s)]
                                (if-let [parent-spec (s/get-spec p)]
                                  (if (keyword? parent-spec) parent-spec p)
                                  p)))))

(defn- safe-type-label [{:keys [v/path] :as n} s]
  (if (= 2 (count path)) (last (butlast path)) (type-label n s)))

(defn- missing-type-error [{:keys [spec] :as n} s]
  (let [type-label (safe-type-label n s)]
    {:error (format "Cannot query field '%s' on type '%s'." (name spec) type-label)
     :loc   (ve/extract-loc (meta n))}))

;; TODO allowed meta field such as __typename?

(defnodevisitor missing-type :pre :field
  [{:keys [spec v/path] :as n} s]
  (when-not (s/get-spec spec)
    {:state (ve/update-errors s (missing-type-error n s))
     :break true}))

(def rules [missing-type])
