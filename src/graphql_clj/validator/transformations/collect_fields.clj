(ns graphql-clj.validator.transformations.collect-fields
  "Collect fields at the same level of a selection set and precompute a select function"
  (:require [graphql-clj.visitor :as v]
            [graphql-clj.spec :as spec]
            [graphql-clj.box :as box]))

(defn- select-fields
  "Note: these keys will be unordered, which is not compliant.
   From spec - `Since the result of evaluating a selection set is ordered,
   the JSON object serialized should preserve this order by writing the object
   properties in the same order as those fields were requested as defined by query execution.`"
  [selection-keys resolved]
  (select-keys resolved selection-keys))

(declare collect-fields)
(v/defnodevisitor collect-fields :pre :field
  [{:keys [selection-set spec] :as n} s]
  (when selection-set
    (let [ks (mapv (comp keyword box/box->val :field-name) selection-set)]
      {:node (assoc n :kind (:kind (spec/get-type-node spec s))
                      :select-fn (partial select-fields ks))})))

(def rules [collect-fields])
