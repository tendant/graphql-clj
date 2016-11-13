(ns graphql-clj.validator.transformations.response-path
  (:require [graphql-clj.visitor :as v]
            [graphql-clj.box :as box]))

(defn- path->response-path
  "Given a path in the parsed statement tree, return a cleaned up path to the response value"
  [path alias]
  (let [path (rest path)] ;; Remove root
    (->> (if alias (conj (butlast path) alias) path)
         (mapv (comp keyword box/box->val)))))

(declare response-path)
(v/defnodevisitor response-path :post :field
  [{:keys [v/path selection-set v/parent] :as n} s]
  {:node (assoc n :v/response-path (path->response-path path (:name n))
                  :v/parent-type-name (or (some-> parent :spec name) (:query-root-name s)))})

(def rules [response-path])
