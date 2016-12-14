(ns graphql-clj.validation
  (:require [clojure.walk :as walk]
            [clojure.zip :as z]
            [clojure.spec :as s]
            [zip.visit :as zv]
            [graphql-clj.parser :as parser]
            [graphql-clj.spec.type-system :as type-system]))

;; Zipper

(def ^:private node-type->children-fn
  {:graphql-clj/type-definition :graphql-clj/type-fields
   :graphql-clj/field :graphql-clj/selection-set
   :graphql-clj/operation-definition :graphql-clj/selection-set
   :graphql-clj/operations-definitions :graphql-clj/operation-definition
   :graphql-clj/interface-definition :graphql-clj/fields
   :graphql-clj/input-definition :graphql-clj/fields
   :graphql-clj/statement-root :graphql-clj/children
   :graphql-clj/fragment-definition :graphql-clj/selection-set
   :graphql-clj/inline-fragment :graphql-clj/selection-set})

(def ^:private node-type->make-node-fn
  (let [assoc-field-fn (fn [k]
                         (fn [node children]
                           (println "update node:" node)
                           (assoc node k children :changed "visited")))]
    {:graphql-clj/type-definition (assoc-field-fn :graphql-clj/type-fields)
     :graphql-clj/field (assoc-field-fn :graphql-clj/selection-set)
     :graphql-clj/operation-definition (assoc-field-fn :graphql-clj/selection-set)
     :graphql-clj/operations-definitions (assoc-field-fn :graphql-clj/operation-definition)
     :graphql-clj/interface-definition (assoc-field-fn :graphql-clj/fields)
     :graphql-clj/input-definition (assoc-field-fn :graphql-clj/fields)
     :graphql-clj/statement-root (assoc-field-fn :graphql-clj/children)
     :graphql-clj/fragment-definition (assoc-field-fn :graphql-clj/selection-set)
     :graphql-clj/inline-fragment (assoc-field-fn :graphql-clj/selection-set)}))

(defn node-branch? [node]
  (assert (:graphql-clj/node-type node) "node-type should not be null for checking 'node-branch?'.")
  (let [node-type (:graphql-clj/node-type node)]
    (println "node-branch?:" (contains? (set (keys node-type->children-fn)) node-type))
    (if (contains? (set (keys node-type->children-fn)) node-type)
      true
      (println "error: node has no branch." node-type))))

(defn node->children
  [node]
  (assert (:graphql-clj/node-type node) "node-type should not be null for node->children!")
  (let [node-type (:graphql-clj/node-type node)
        children-fn (get node-type->children-fn node-type)]
    (if children-fn
      (children-fn node)
      (println "warn: no children-fn found for node-type:" node-type))))

(defn make-node-with-node-type
  [node children]
  (assert (:graphql-clj/node-type node) "node-type should not be null for make-node-with-node-type!")
  (let [node-type (:graphql-clj/node-type node)
        make-node-fn (get node-type->make-node-fn node-type)]
    (if make-node-fn
      (make-node-fn node children)
      (println "error: make-node-fn not found!" node-type))))

(defn branch? [node]
  (println "branch?" (keys node) "node-type:" (:graphql-clj/node-type node))
  (cond
    (:graphql-clj/operation-definitions node) true
    (:graphql-clj/type-system-definitions node) true
    (:graphql-clj/node-type node) (node-branch? node)
    :default (do
               (println "error: don't know how to create branch for node:" node)
               false)))

(defn make-node-fn
  [node children]
  (println "make-node-fn:" (keys node))
  (if (map? node)
    (cond
      (:graphql-clj/operation-definitions node) (assoc node :graphql-clj/operation-definitions children)
      (:graphql-clj/type-system-definitions node) (assoc node :graphql-clj/type-system-definitions children)
      (:graphql-clj/node-type node) (make-node-with-node-type node children)
      :default (println "error: node is not a map!" node))
    (println "error: don't know how to make node from:" node "--- map?:" (map? node))))

(defn children
  [node]
  (println "children:" (keys node))
  (cond
    (:graphql-clj/operation-definitions node) (do
                                                (:graphql-clj/operation-definitions node))
    (:graphql-clj/type-system-definitions node) (:graphql-clj/type-system-definitions node)
    (:graphql-clj/node-type node) (node->children node)
    :default (do
               (println "error: no children for node:" node)
               nil)))

(defn document-zipper
  [document]
  (z/zipper branch? children make-node-fn document))

(zv/defvisitor type-field-visitor :pre [n s]
  (if (= :graphql-clj/type-field (:graphql-clj/node-type n))
    {:node (assoc n :changed "visited")
     :state s}))

(defn validate-schema
  [document]
  (assert (s/conform :graphql-clj/type-system document))
  (zv/visit (document-zipper document) {} [type-field-visitor]))

(defn validate-query
  [parsed-query]
  (assert (s/conform :graphql-clj/query parsed-query)))

(comment
  (def schema-str "enum DogCommand { SIT, DOWN, HEEL }

type Dog implements Pet {
  name: String!
  nickname: String
  barkVolume: Int
  doesKnowCommand(dogCommand: DogCommand!): Boolean!
  isHousetrained(atOtherHomes: Boolean): Boolean!
  owner: Human
}

interface Sentient {
  name: String!
}

interface Pet {
  name: String!
}

type Alien implements Sentient {
  name: String!
  homePlanet: String
}

type Human implements Sentient {
  name: String!
}

enum CatCommand { JUMP }

type Cat implements Pet {
  name: String!
  nickname: String
  doesKnowCommand(catCommand: CatCommand!): Boolean!
  meowVolume: Int
}

union CatOrDog = Cat | Dog
union DogOrHuman = Dog | Human
union HumanOrAlien = Human | Alien

type QueryRoot {
  dog: Dog
}"))
