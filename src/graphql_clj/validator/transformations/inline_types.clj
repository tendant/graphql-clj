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

(declare inline-types)
(v/defnodevisitor inline-types :pre :field
  [{:keys [spec v/path v/parent] :as n} s]
  (let [{:keys [kind] :as base} (spec/get-base-type-node spec s)
        parent-type-name (or  (some-> parent parent-type name) (first path))]
    {:node (cond-> n
                   kind             (assoc :kind kind)
                   (= :LIST kind)   (assoc :of-kind (of-kind base s))
                   parent-type-name (assoc :parent-type-name parent-type-name))}))

(def rules [inline-types])
