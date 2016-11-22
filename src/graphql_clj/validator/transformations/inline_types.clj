(ns graphql-clj.validator.transformations.inline-types
  "Inline field and parent field types for execution phase"
  (:require [graphql-clj.visitor :as v]
            [graphql-clj.box :as box]))

(def whitelisted-keys
  #{:v/parentk :node-type :selection-set :type-name :field-name :name :args-fn :kind :of-kind :required :parent-type-name})

(declare inline-types)
(v/defnodevisitor inline-types :post :field
  [{:keys [field-name parent-type-name spec v/path v/parent] :as n} {:keys [resolver] :as s}]
  {:node (cond-> (select-keys n whitelisted-keys)
                 parent-type-name (assoc :parent-type-name (box/box->val parent-type-name))
                 resolver (assoc :resolver-fn (resolver parent-type-name field-name)))})

(def rules [inline-types])
