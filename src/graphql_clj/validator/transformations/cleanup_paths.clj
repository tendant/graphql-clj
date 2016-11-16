(ns graphql-clj.validator.transformations.cleanup-paths
  (:require [graphql-clj.visitor :as v]))

(declare cleanup)
(v/defmapvisitor cleanup :post [n s]
  (when (:node-type n)
    {:node (dissoc n :v/parent :v/path)}))

(def rules [cleanup])

