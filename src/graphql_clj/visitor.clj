(ns graphql-clj.visitor
  (:require [clojure.zip :as z]
            [zip.visit :as zv]))

;; Mappings

(def ^:private node-type->children
  "Mapping from a node type to a set of keys that contain "
  {:type-definition        #{:fields}
   :type-field             #{:arguments}
   :field                  #{:arguments :selection-set}
   :operation-definition   #{:selection-set :variable-definitions}
   :operations-definitions #{:operation-definition}
   :interface-definition   #{:fields}
   :input-definition       #{:fields}})

(def ^:private node-type->label-key
  {:type-definition      [:type-name]
   :type-field           [:field-name]
   :field                [:field-name]
   :argument             [:argument-name]
   :variable-definition  [:variable-name]
   :interface-definition [:type-name]
   :enum-definition      [:type-name]
   :type-field-argument  [:argument-name]
   :list                 [:field-name :argument-name]
   :input-type-field     [:field-name]
   :input-definition     [:type-name]})

;; Zipper

(def ^:private keys-with-children
  (reduce (fn [acc [_ v]] (into acc v)) #{} node-type->children))

(def ^:private has-children?
  (apply some-fn keys-with-children))

(def ^:private node-type->label-key-fn
  (->> (map (fn [[k v]] [k (apply some-fn v)]) node-type->label-key) (into {})))

(defn- branch? [node]
  (or (vector? node) (and (map? node) (has-children? node))))

(defn- node->label [{:keys [node-type] :as node}]
  (when-let [label-fn (get node-type->label-key-fn node-type)]
    (label-fn node)))

(defn- parent-path [parent]
  (or (:v/path parent) (if-let [label (node->label parent)] [label] [])))

(defn- add-path [parentk parent child]
  (assoc child :v/parentk parentk
               :v/path (conj (parent-path parent) (node->label child))))

(defn- children-w-path [node f]
  (->> (f node) (map (partial add-path f node))))

(defn- children [node]
  (cond (vector? node) node
        (map? node) (->> (get node-type->children (:node-type node))
                         (mapcat (partial children-w-path node)))))

(defn- make-node [node children]
  (if (map? node)
    (merge node (group-by :v/parentk children))
    children))

(def ^:private zipper (partial z/zipper branch? children make-node))

(defn- visit
  "Returns a map with 2 keys: :node contains the visited tree, :state contains the accumulated state"
  [ast visitor-fns]
  (zv/visit (zipper ast) nil visitor-fns))

(def ^:private document-sections [:type-system-definitions
                                  :fragment-definitions
                                  :operation-definitions])

;; Public API

(defn visit-document [document visitor-fns]
  (reduce #(update %1 %2 visit visitor-fns) document document-sections))
