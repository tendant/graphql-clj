(ns graphql-clj.execution
  (:require [com.walmartlabs.lacinia :as lacinia]))

(defn execute [schema query variables context]
  (lacinia/execute schema query variables context))