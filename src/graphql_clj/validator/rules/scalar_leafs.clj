(ns graphql-clj.validator.rules.scalar-leafs
  "A GraphQL document is valid only if all leaf fields (fields without sub selections) are of scalar or enum types."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]
            [graphql-clj.spec :as spec]))

(defn- required-subselection-error [{:keys [field-name base-spec]}]
  {:error (format "Field '%s' of type '%s' must have a selection of subfields." field-name (name base-spec))
   :loc   (ve/extract-loc (meta field-name))})

(defn- no-subselection-allowed-error [{:keys [field-name base-spec]}]
  {:error (format "Field '%s' must not have a selection since type '%s' has no subfields." field-name (name base-spec))
   :loc   (ve/extract-loc (meta field-name))})

(def scalar-kinds #{:SCALAR :ENUM})

(defn scalar? [base-spec kind]
  (or (spec/default-spec-keywords base-spec)
      (scalar-kinds kind)))

(defnodevisitor non-scalar-leaf :pre :field
  [{:keys [base-spec selection-set kind] :as n} s]
  (let [scalar? (scalar? base-spec kind)]
    (cond (and scalar? selection-set)
          {:state (ve/update-errors s (no-subselection-allowed-error n))
           :break true}
          (and (not scalar?) (not selection-set))
          {:state (ve/update-errors s (required-subselection-error n))
           :break true})))

(def rules [non-scalar-leaf])
