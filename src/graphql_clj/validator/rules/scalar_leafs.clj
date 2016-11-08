(ns graphql-clj.validator.rules.scalar-leafs
  "A GraphQL document is valid only if all leaf fields (fields without sub selections) are of scalar or enum types."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]
            [clojure.spec :as s]
            [graphql-clj.spec :as spec]))

(defn- field-type+spec->label [field-type spec]
  (if (keyword? field-type) (name field-type) (name spec)))

(defn- required-subselection-error [field-name field-type spec]
  {:error (format "Field '%s' of type '%s' must have a selection of subfields." field-name (field-type+spec->label field-type spec))
   :loc   (ve/extract-loc (meta field-name))})

(defn- no-subselection-allowed-error [field-name field-type spec]
  {:error (format "Field '%s' must not have a selection since type '%s' has no subfields." field-name (field-type+spec->label field-type spec))
   :loc   (ve/extract-loc (meta field-name))})

(def scalar-kinds #{:SCALAR :ENUM})

(defnodevisitor non-scalar-leaf :pre :field
  [{:keys [field-name spec selection-set] :as n} s]
  (let [field-type (s/get-spec spec)
        scalar? (or (spec/default-spec-keywords field-type) ;; TODO messy
                    (scalar-kinds (:kind (or (spec/get-type-node field-type s)
                                             (spec/get-type-node spec s)))))]
    (cond (and scalar? selection-set)
          {:state (ve/update-errors s (no-subselection-allowed-error field-name field-type spec))
           :break true}

          (and (not scalar?) (not selection-set))
          {:state (ve/update-errors s (required-subselection-error field-name field-type spec))
           :break true})))

(def rules [non-scalar-leaf])
