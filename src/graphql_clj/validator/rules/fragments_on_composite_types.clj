(ns graphql-clj.validator.rules.fragments-on-composite-types
  "Fragments use a type condition to determine if they apply, since fragments
   can only be spread into a composite type (object, interface, or union), the
   type condition must also be a composite type."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]))

(defn- inline-composite-type-error [{:keys [base-spec] :as n}]
  {:error (format "Fragment cannot condition on non composite type '%s'." (name base-spec))
   :loc   (ve/extract-loc (meta n))})

(defn- composite-type-error [{:keys [spec base-spec] :as n}]
  {:error (format "Fragment '%s' cannot condition on non composite type '%s'." (name spec) (name base-spec))
   :loc   (ve/extract-loc (meta n))})

(def acceptable-kinds #{:OBJECT :INTERFACE :UNION})

(defnodevisitor fragment-type-inline :pre :inline-fragment
  [{:keys [spec kind] :as n} s]
  (when-not (acceptable-kinds kind)
    {:state (ve/update-errors s (inline-composite-type-error n))
     :break true}))

(defnodevisitor fragment-type-def :pre :fragment-definition
  [{:keys [spec kind] :as n} s]
  (when-not (acceptable-kinds kind)
    {:state (ve/update-errors s (composite-type-error n))
     :break true}))

(def rules [fragment-type-inline
            fragment-type-def])
