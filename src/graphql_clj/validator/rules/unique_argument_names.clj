(ns graphql-clj.validator.rules.unique-argument-names
  "A GraphQL field or directive is only valid if all supplied arguments are uniquely named."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]))

(defnodevisitor duplicate-argument-name-type-field :pre :type-field [n s]
  (ve/guard-duplicate-names "argument" :argument-name (:arguments n) s))

(defnodevisitor duplicate-argument-name-field :pre :field [n s]
  (ve/guard-duplicate-names "argument" :argument-name (:arguments n) s))

(defnodevisitor duplicate-argument-name-directive :pre :directive [n s]
  (ve/guard-duplicate-names "argument" :argument-name (:arguments n) s))

(def rules [duplicate-argument-name-type-field
            duplicate-argument-name-field
            duplicate-argument-name-directive])
