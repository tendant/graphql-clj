(ns graphql-clj.validator.rules.scalar-leafs
  "A GraphQL document is valid only if all leaf fields (fields without sub selections) are of scalar or enum types."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]
            [clojure.spec :as s]
            [graphql-clj.spec :as spec]))

(defn- type-label [field-type spec]
  (if (keyword? field-type) (name field-type) (name spec)))

(defn- required-subselection-error [field-name field-type spec base-type]
  (let [label (type-label field-type (type-label (:spec base-type) spec))]
    {:error (format "Field '%s' of type '%s' must have a selection of subfields." field-name label)
     :loc   (ve/extract-loc (meta field-name))}))

(defn- no-subselection-allowed-error [field-name field-type spec]
  (let [label (type-label field-type spec)]
    {:error (format "Field '%s' must not have a selection since type '%s' has no subfields." field-name label)
     :loc   (ve/extract-loc (meta field-name))}))

(def scalar-kinds #{:SCALAR :ENUM})

(defn get-type-node [field-type spec s]
  (or (spec/get-type-node field-type s)
      (spec/get-type-node spec s)))

(defn base-type [field-type spec s]
  (let [{:keys [kind inner-type] :as type-node} (get-type-node field-type spec s)
        inner-spec (some->> inner-type :type-name name vector (spec/named-spec s))]
    (cond (spec/default-spec-keywords spec) spec
          (spec/default-spec-keywords field-type) field-type
          (spec/default-spec-keywords inner-spec) inner-spec
          (= :LIST kind) (spec/get-type-node inner-spec s)
          :else type-node)))

(defn scalar? [base-type]
  (or (spec/default-spec-keywords base-type)
      (scalar-kinds (:kind base-type))))

(defnodevisitor non-scalar-leaf :pre :field
  [{:keys [field-name spec selection-set] :as n} s]
  (let [field-type (s/get-spec spec)
        base-type (base-type field-type spec s)
        scalar?   (scalar? base-type)]
    (cond (and scalar? selection-set)
          {:state (ve/update-errors s (no-subselection-allowed-error field-name field-type spec))
           :break true}

          (and (not scalar?) (not selection-set))
          {:state (ve/update-errors s (required-subselection-error field-name field-type spec base-type))
           :break true})))

(def rules [non-scalar-leaf])
