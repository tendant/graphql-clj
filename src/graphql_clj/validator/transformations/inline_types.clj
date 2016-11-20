(ns graphql-clj.validator.transformations.inline-types
  "Inline field and parent field types for execution phase"
  (:require [graphql-clj.visitor :as v]
            [graphql-clj.spec :as spec]
            [clojure.spec :as s]
            [graphql-clj.box :as box]))

(defn- of-kind [{:keys [kind required inner-type]} s]
  (if (:type-name inner-type)
    (let [base (spec/get-type-node (spec/named-spec s [(:type-name inner-type)]) s)]
      (select-keys base [:kind :required]))
    {:kind kind :required required :of-kind (of-kind inner-type s)}))

(defn- parent-type [{:keys [spec]}]
  (if-let [base-spec (s/get-spec spec)]
    (if (keyword? base-spec) base-spec spec)
    spec))

(def whitelisted-keys
  #{:v/parentk :node-type :selection-set :type-name :field-name :name :args-fn :kind :of-kind :required :parent-type-name})

(declare inline-types)
(v/defnodevisitor inline-types :post :field
  [{:keys [field-name parent-type-name spec v/path v/parent] :as n} {:keys [resolver] :as s}]
  {:node (cond-> (select-keys n whitelisted-keys)
                 parent-type-name (assoc :parent-type-name (box/box->val parent-type-name))
                 resolver (assoc :resolver-fn (resolver parent-type-name field-name)))})

(def rules [inline-types])
