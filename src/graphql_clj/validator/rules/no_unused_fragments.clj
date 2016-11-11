(ns graphql-clj.validator.rules.no-unused-fragments
  "A GraphQL document is only valid if all fragment definitions are spread
   within operations, or spread within other fragments spread within operations."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]
            [clojure.set :as set]
            [graphql-clj.box :as box]))

(defn- unused-fragment-error [{:keys [operation-type]} fragment-name]
  {:error (if (:name operation-type)
            (format "Fragment '%s' is never used in operation '%s'." fragment-name (:name operation-type))
            (format "Fragment '%s' is never used." fragment-name))
   :loc (ve/extract-loc (meta fragment-name))})

(defnodevisitor fragment-def :pre :fragment-definition
  [{:keys [name] :as n} s]
  (when name
    {:state (update s :frag-defs conj (box/->Box name (meta n)))}))

(defnodevisitor fragment-use :pre :fragment-spread
  [{:keys [name] :as n} s]
  (when name
    {:state (update s :frag-usages conj (box/->Box name (meta n)))}))

(defnodevisitor unused-fragments :post :operation-definition
  [n {:keys [frag-defs frag-usages] :as s}]
  (let [delta (set/difference (set frag-defs) (set frag-usages))]
    (when-not (empty? delta)
      {:state (apply ve/update-errors s (map #(unused-fragment-error n %) delta))})))

(def rules [fragment-def
            fragment-use
            unused-fragments])
