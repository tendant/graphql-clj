(ns graphql-clj.validator.transformations.unbox
  (:require [graphql-clj.visitor :as v]
            [graphql-clj.box :as box]))

(declare unbox)
(v/defmapvisitor unbox :post [n s]
  (when (:node-type n)
    (cond-> {:node n}
            (:type-name n)     (update-in [:node :type-name]     box/box->val)
            (:field-name n)    (update-in [:node :field-name]    box/box->val)
            (:argument-name n) (update-in [:node :argument-name] box/box->val)
            (:variable-name n) (update-in [:node :variable-name] box/box->val)
            (:implements n)    (update-in [:node :implements :type-names] #(mapv box/box->val %))
            (:type-names n)    (update-in [:node :type-names] #(mapv box/box->val %))
            (:inner-type n)    (update-in [:node :inner-type :type-name] box/box->val)
            (:on n)            (update-in [:node :on :type-name] box/box->val)
            (:value n)         (update-in [:node :value] box/box->val))))

(def rules [unbox])
