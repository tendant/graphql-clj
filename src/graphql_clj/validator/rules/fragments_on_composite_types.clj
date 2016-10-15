(ns graphql-clj.validator.rules.fragments-on-composite-types
  "Fragments use a type condition to determine if they apply, since fragments
   can only be spread into a composite type (object, interface, or union), the
   type condition must also be a composite type."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]
            [clojure.spec :as s]
            [graphql-clj.spec :as spec]))

(defn- composite-type-error [{:keys [spec]} s]
  (format "Fragment cannot condition on non composite type '%s'." (name spec)))

(def acceptable-kinds #{:OBJECT :INTERFACE :UNION})

(defnodevisitor fragment-type :pre :inline-fragment
  [{:keys [spec] :as n} s]
  (when-not (acceptable-kinds (:kind (spec/get-type-node spec s)))
    {:state (ve/update-errors s (composite-type-error n s))}))

(def rules [fragment-type])
