(ns graphql-clj.validator.transformations.response-path
  "Define destination path into the response value for each field"
  (:require [graphql-clj.visitor :as v]
            [graphql-clj.box :as box]
            [clojure.spec :as s]))

(defn- path->response-path
  "Given a path in the parsed statement tree, return a cleaned up path to the response value"
  [path alias]
  (let [path (rest path)] ;; Remove root, it's not used for the response value
    (->> (if alias (conj (butlast path) alias) path)
         (mapv (comp keyword box/box->val)))))

(defn- parent-type [{:keys [spec]}]
  (if-let [base-spec (s/get-spec spec)]
    (if (keyword? base-spec) base-spec spec)
    spec))

(declare response-path)
(v/defnodevisitor response-path :post :field
  [{:keys [v/path v/parent] :as n} s]
  {:node (assoc n :v/response-path (path->response-path path (:name n))
                  :v/parent-type-name (or  (some-> parent parent-type name) (first path)))})

(def rules [response-path])
