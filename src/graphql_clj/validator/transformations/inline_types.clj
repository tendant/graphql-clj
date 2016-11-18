(ns graphql-clj.validator.transformations.inline-types
  "Inline field and parent field types for execution phase"
  (:require [graphql-clj.visitor :as v]
            [graphql-clj.spec :as spec]
            [clojure.spec :as s]))

(defn- of-kind [{:keys [kind required inner-type]} s]
  (if (:type-name inner-type)
    (let [base (spec/get-type-node (spec/named-spec s [(:type-name inner-type)]) s)]
      (select-keys base [:kind :required]))
    {:kind kind :required required :of-kind (of-kind inner-type s)}))

(defn- parent-type [{:keys [spec]}]
  (if-let [base-spec (s/get-spec spec)]
    (if (keyword? base-spec) base-spec spec)
    spec))

(def whitelisted-keys #{:v/parentk :node-type :selection-set :type-name :field-name :name :args-fn})

(declare inline-types)
(v/defnodevisitor inline-types :post :field
  [{:keys [field-name spec v/path v/parent] :as n} {:keys [resolver] :as s}]
  (let [{:keys [kind required] :as base} (spec/get-base-type-node spec s)
        parent-type-name (or  (some-> parent parent-type name) (first path))]
    {:node (cond-> (select-keys n whitelisted-keys)
                   kind             (assoc :kind kind)
                   required         (assoc :required required)
                   (= :LIST kind)   (assoc :of-kind (of-kind base s))
                   parent-type-name (assoc :parent-type-name parent-type-name)
                   resolver         (assoc :resolver-fn (resolver parent-type-name field-name)))}))

(def rules [inline-types])
