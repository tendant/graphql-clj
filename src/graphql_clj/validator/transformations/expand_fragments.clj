(ns graphql-clj.validator.transformations.expand-fragments
  "Expand fragment spreads to normal fields to simplify execution"
  (:require [graphql-clj.visitor :as v]
            [graphql-clj.spec :as spec]
            [clojure.spec :as s]))

(defn- expand-fragment-spread [n s]
  (let [frag-def (spec/get-type-node (:spec n) s)]
    (map #(assoc % :v/parent {:spec (s/get-spec (:spec n))}) (:selection-set frag-def))))

(declare expand-fragments)
(v/defnodevisitor expand-fragments :pre :field
  [{:keys [selection-set spec v/path] :as n} s]
  (when selection-set
    (let [[frag-spreads fields] (split-with #(= :fragment-spread (:node-type %)) (:selection-set n))
          expanded-frag-spreads (mapcat #(expand-fragment-spread % s) frag-spreads)]
      {:node  (assoc n  :selection-set (into expanded-frag-spreads fields))})))

(def rules [expand-fragments])
