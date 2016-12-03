(ns graphql-clj.validation
  (:require [clojure.walk :as walk]
            [clojure.zip :as z]
            [zip.visit :as zv]
            [graphql-clj.parser :as parser]))

(defn- verify-node [node ancestor]
  (if (= :type-definition (:node-type node))
    (do
      (println "type-definition:", (:node-type node))
      node)
    node))

(defn validate-query-document
  [query-document]
  (let [ancestor (atom [])])
  (walk/prewalk verify-node query-document))

;; Zipper

(defn node->branch? [node]
  (case (:node-type node)
    :schema-definition false
    :type-definition true
    (println "error: unknow node-type for branch:" (:node-type node))))

(defn make-node-with-node-type
  [node children]
  (case (:node-type node)
    ;; :operation-definition (assoc node :selection-set children)
    :type-definition (assoc node :fields children :changed "visited")
    (println "error: don't know how to create node for:" (:node-type node))))

(defn node->children
  [node]
  (println "node->children:" (:node-type node))
  (case (:node-type node)
    :type-definition (:fields node)
    (println "error: don't know to make children from:" node)))

(defn branch? [node]
  (println "branch?")
  (cond
    (:operation-definitions node) true
    (:type-system-definitions node) true
    (:node-type node) (node->branch? node)
    :default (do
               (println "error no branch for node:" node)
               false)))

(defn make-node-fn
  [node children]
  (println "make-node-fn:" (or (:operation-definitions node)
                               (:node-type node)
                               (:type-system-definitions node)))
  (if (map? node)
    (cond
      (:operation-definitions node) (assoc node :operation-definitions children)
      (:type-system-definitions node) (assoc node :type-system-definitions children)
      (:node-type node) (make-node-with-node-type node children)
      :default node)
    (println "error: don't know how to make node from:" node "--- map?:" (map? node))))

(defn children
  [node]
  (println "children")
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

(defn debug [& args]
  (println "args:" args))

(defn visit
  [document]
  (zv/visit (document-zipper document) {} [debug]))

