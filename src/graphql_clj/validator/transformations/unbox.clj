(ns graphql-clj.validator.transformations.unbox
  (:require [graphql-clj.visitor :as v]
            [graphql-clj.box :as box]))

(defn- unbox-node [n]
  (cond-> n
          (:type-name n)      (update :type-name     box/box->val)
          (:field-name n)     (update :field-name    box/box->val)
          (:argument-name n)  (update :argument-name box/box->val)
          (:variable-name n)  (update :variable-name box/box->val)
          (:implements n)     (update-in [:implements :type-names] #(mapv box/box->val %))
          (:type-condition n) (update-in [:type-condition :type-name] box/box->val)
          (:type-names n)     (update :type-names #(mapv box/box->val %))
          (:inner-type n)     (update-in [:inner-type :type-name] box/box->val)
          (:on n)             (update-in [:on :type-name] box/box->val)
          (:value n)          (update :value box/box->val)))

(declare unbox)
(v/defmapvisitor unbox :post [n s]
  (when (:node-type n) {:node (unbox-node n)}))

(def rules [unbox])
