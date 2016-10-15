(ns graphql-clj.validator.rules.no-fragment-cycles
  "A GraphQL document is only valid if fragment spreads do not form cycles"
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]
            [clojure.string :as str]))

(defn- fragment-cycle-error [{:keys [name]} spread-path]
  (let [joined (when (> (count spread-path) 1)
                 (str " via '" (str/join "," (rest spread-path)) "'"))]
    (format "Cannot spread fragment '%s' within itself%s." name joined)))

(defn- accumulate-errors [s errors]
  (apply ve/update-errors s (map #(fragment-cycle-error % (:spread-path s)) errors)))

(declare detect-cycles)

(defn- recur-detect-cycles [s errors {:keys [spec]}]
  (into errors (detect-cycles (get-in s [:spec-map spec]) s))) ;; TODO stackoverflow risk?

(defn- get-spread-nodes [{:keys [selection-set]}]
  (let [spread-nodes (filter #(= :fragment-spread (:node-type %)) selection-set)
        fields (filter #(= :field (:node-type %)) selection-set)]
    (reduce #(into %1 (get-spread-nodes %2)) spread-nodes fields))) ;; TODO stackoverflow risk?

(defn detect-cycles
  [{:keys [name] :as n} {:keys [visited-frags] :as s}]
  (let [spread-nodes (get-spread-nodes n)]
    (if (empty? spread-nodes)
      (:errors s)
      (let [[errors non-errors] (split-with #(contains? visited-frags (:name %)) spread-nodes)
            s' (-> (update s :visited-frags conj name)
                   (update :spread-path conj name)
                   (accumulate-errors errors))]
        (reduce (partial recur-detect-cycles s') (:errors s') non-errors)))))

(defnodevisitor fragment-cycles :pre :fragment-definition [n s]
  (when-let [cycles (detect-cycles n (assoc s :visited-frags #{} :spread-path []))]
    {:state (update s :errors into cycles)
     :break true}))

(def rules [fragment-cycles])
