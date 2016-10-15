(ns graphql-clj.validator.rules.known-argument-names
  "A GraphQL field is only valid if all supplied arguments are defined by that field."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]
            [clojure.spec :as s]
            [graphql-clj.spec :as spec]))

(defn- undefined-argument-error [{:keys [argument-name v/parent] :as n} s]
  (format "Unknown argument '%s' on field '%s' of type '%s'."
          argument-name (name (:spec parent)) (:type-name (spec/get-type-node (spec/get-parent-type n s) s))))

(defnodevisitor undefined-argument :pre :argument
  [{:keys [spec value v/path v/parent] :as n} s]
  (when-not (s/get-spec spec)
    (when (= (:node-type parent) :field) ;; TODO directives can have arguments too
      {:state (ve/update-errors s (undefined-argument-error n s))
       :break true})))

(def rules [undefined-argument])
