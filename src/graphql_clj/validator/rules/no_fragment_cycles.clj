(ns graphql-clj.validator.rules.no-fragment-cycles
  "A GraphQL document is only valid if fragment spreads do not form cycles"
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]
            [clojure.string :as str]
            [graphql-clj.spec :as spec]))

(defn- fragment-cycle-error [{:keys [name] :as n} spread-path]
  (let [joined (when (> (count spread-path) 1) (str " via '" (str/join "," (rest spread-path)) "'"))]
    {:error (format "Cannot spread fragment '%s' within itself%s." name joined)
     :loc   (ve/extract-loc (meta n))}))

(defn- accumulate-errors [s errors]
  (apply ve/update-errors s (map #(fragment-cycle-error % (:spread-path s)) errors)))

(defn- get-spread-nodes
  "Recursively gather fragment spreads from the current selection-set, and any selection sets nested within fields"
  [{:keys [selection-set]}]
  (let [[fields spread-nodes] (split-with #(= :field (:node-type %)) selection-set)]
    (reduce #(into %1 (get-spread-nodes %2)) spread-nodes fields))) ;; TODO stackoverflow risk?

(defn- detect-cycles
  "Recursively search for child fragments that reference the given parent fragment-definition"
  [{:keys [name] :as n} {:keys [visited-frags] :as s}]
  (let [spread-nodes (get-spread-nodes n)]
    (if (empty? spread-nodes)
      (:errors s)
      (let [[errors non-errors] (split-with #(contains? visited-frags (:name %)) spread-nodes)
            s' (-> (update s :visited-frags conj name)
                   (update :spread-path conj name)
                   (accumulate-errors errors))]
        (reduce #(into %1 (detect-cycles (spec/get-type-node (:spec %2) s') s')) (:errors s') non-errors))))) ;; TODO stackoverflow risk?

(defnodevisitor fragment-cycles :pre :fragment-definition [n s]
  (when-let [cycles (detect-cycles n (assoc s :visited-frags #{} :spread-path []))]
    (when-not (empty? cycles)
      {:state (update s :errors into cycles)
       :break true})))

(def rules [fragment-cycles])
