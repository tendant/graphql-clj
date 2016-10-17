(ns graphql-clj.validator.rules.scalar-leafs
  "A GraphQL document is valid only if all leaf fields (fields without sub selections) are of scalar or enum types."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]
            [clojure.spec :as s]
            [graphql-clj.spec :as spec]))

(defn- required-subselection-error [field-name field-type]
  (format "Field '%s' of type '%s' must have a selection of subfields." field-name (name field-type)))

(defn- no-subselection-allowed-error [field-name field-type]
  (format "Field '%s' must not have a selection since type '%s' has no subfields." field-name (name field-type)))

(def scalar-kinds #{:SCALAR :ENUM})

(defnodevisitor non-scalar-leaf :pre :field
  [{:keys [field-name spec selection-set] :as n} s]
  (let [field-type (s/get-spec spec)
        scalar? (or (spec/default-spec-keywords field-type) ;; TODO messy
                    (scalar-kinds (:kind (or (spec/get-type-node field-type s)
                                             (spec/get-type-node spec s)))))]
    (cond (and scalar? selection-set)
          {:state (ve/update-errors s (no-subselection-allowed-error field-name field-type))
           :break true}

          (and (not scalar?) (not selection-set))
          {:state (ve/update-errors s (required-subselection-error field-name field-type))
           :break true})))

(def rules [non-scalar-leaf])
