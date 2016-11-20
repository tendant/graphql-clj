(ns graphql-clj.validator.rules.fragments-on-composite-types
  "Fragments use a type condition to determine if they apply, since fragments
   can only be spread into a composite type (object, interface, or union), the
   type condition must also be a composite type."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]
            [graphql-clj.spec :as spec]))

(defn- composite-type-error
  ([{:keys [spec] :as n}]
   {:error (format "Fragment cannot condition on non composite type '%s'." (name spec))
    :loc   (ve/extract-loc (meta n))})
  ([{:keys [spec] :as n} base-type-node]
   {:error (format "Fragment '%s' cannot condition on non composite type '%s'." (name spec) (name (:spec base-type-node)))
    :loc   (ve/extract-loc (meta n))}))

(def acceptable-kinds #{:OBJECT :INTERFACE :UNION})

(defnodevisitor fragment-type-inline :pre :inline-fragment
  [{:keys [spec] :as n} s]
  (when-not (acceptable-kinds (:kind (spec/get-type-node spec s)))
    {:state (ve/update-errors s (composite-type-error n))
     :break true}))

(defnodevisitor fragment-type-def :pre :fragment-definition
  [{:keys [spec] :as n} s]
  (let [base-type-node (spec/get-base-type-node spec s)]
    (when-not (acceptable-kinds (:kind base-type-node))
      {:state (ve/update-errors s (composite-type-error n base-type-node))
       :break true})))

(def rules [fragment-type-inline
            fragment-type-def])
