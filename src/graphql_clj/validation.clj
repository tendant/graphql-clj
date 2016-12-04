(ns graphql-clj.validation
  (:require [clojure.walk :as walk]
            [clojure.zip :as z]
            [clojure.spec :as s]
            [zip.visit :as zv]
            [graphql-clj.parser :as parser]
            [graphql-clj.spec.type-system :as type-system]))

;; Zipper

(def ^:private node-type->children-fn
  {:type-definition :fields
   :field :selection-set
   :operation-definition :selection-set
   :operations-definitions :operation-definition
   :interface-definition :fields
   :input-definition :fields
   :statement-root :children
   :fragment-definition :selection-set
   :inline-fragment :selection-set})

(def ^:private node-type->make-node-fn
  (let [assoc-field-fn (fn [k]
                         (fn [node children]
                           (assoc node k children :changed "visited")))]
    {:type-definition (assoc-field-fn :fields)
     :field (assoc-field-fn :selection-set)
     :operation-definition (assoc-field-fn :selection-set)
     :operations-definitions (assoc-field-fn :operation-definition)
     :interface-definition (assoc-field-fn :fields)
     :input-definition (assoc-field-fn :fields)
     :statement-root (assoc-field-fn :children)
     :fragment-definition (assoc-field-fn :selection-set)
     :inline-fragment (assoc-field-fn :selection-set)}))

(defn node-branch? [node]
  (assert (:node-type node) "node-type should not be null for checking 'node-branch?'.")
  (let [node-type (:node-type node)]
    (if (contains? (set (keys node-type->children-fn)) node-type)
      true
      (println "error: node has no branch." node-type))))

(defn node->children
  [node]
  (assert (:node-type node) "node-type should not be null for node->children!")
  (let [node-type (:node-type node)
        children-fn (get node-type->children-fn node-type)]
    (if children-fn
      (children-fn node)
      (println "warn: no children-fn found for node-type:" node-type))))

(defn make-node-with-node-type
  [node children]
  (assert (:node-type node) "node-type should not be null for make-node-with-node-type!")
  (let [node-type (:node-type node)
        make-node-fn (get node-type->make-node-fn node-type)]
    (if make-node-fn
      (make-node-fn node children)
      (println "error: make-node-fn not found!" node-type))))

(defn branch? [node]
  (println "branch?" (keys node) "node-type:" (:node-type node))
  (cond
    (:operation-definitions node) true
    (:type-system-definitions node) true
    (:node-type node) (node-branch? node)
    :default (do
               (println "error: don't know how to create branch for node:" node)
               false)))

(defn make-node-fn
  [node children]
  (println "make-node-fn:" (keys node))
  (if (map? node)
    (cond
      (:operation-definitions node) (assoc node :operation-definitions children)
      (:type-system-definitions node) (assoc node :type-system-definitions children)
      (:node-type node) (make-node-with-node-type node children)
      :default (println "error: node is not a map!" node))
    (println "error: don't know how to make node from:" node "--- map?:" (map? node))))

(defn children
  [node]
  (println "children:" (keys node))
  (cond
    (:operation-definitions node) (do
                                    (:operation-definitions node))
    (:type-system-definitions node) (:type-system-definitions node)
    (:node-type node) (node->children node)
    :default (do
               (println "error: no children for node:" node)
               nil)))

(defn document-zipper
  [document]
  (z/zipper branch? children make-node-fn document))

(zv/defvisitor type-field-visitor :pre [n s]
  (if (= :type-field (:node-type n))
    {:node (assoc n :changed "visited")
     :state s}))

(defn visit
  [document]
  (assert (s/conform :graphql-clj/type-system document))
  (zv/visit (document-zipper document) {} [type-field-visitor]))

