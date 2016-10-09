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
   :input-definition       #{:fields}
   :query-root             #{:children}})

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

(defn- node->label [query-root-mapping {:keys [node-type] :as node}]
  (when-let [label-fn (get node-type->label-key-fn node-type)]
    (let [label (label-fn node)
          label (get (last query-root-mapping) label label)]
      (when-not (= label (first query-root-mapping))
        label))))

(defn- parent-path [query-root-mapping parent]
  (or (:v/path parent) (if-let [label (node->label query-root-mapping parent)] [label] [])))

(defn conj-child-path [query-root-mapping child parent-path]
  (let [child-label (node->label query-root-mapping child)]
    (if (and child-label (not= child-label (first parent-path)))
      (conj parent-path child-label)
      parent-path)))

(defn- add-path [query-root-mapping parentk parent child]
  (assoc child :v/parentk parentk
               :v/path (->> (parent-path query-root-mapping parent)
                            (conj-child-path query-root-mapping child))))

(defn- children-w-path [query-root-mapping node f]
  (->> (f node) (map (partial add-path query-root-mapping f node))))

(defn- children [query-root-mapping node]
  (cond (vector? node) node
        (map? node) (->> (get node-type->children (:node-type node))
                         (mapcat (partial children-w-path query-root-mapping node)))))

(defn- make-node [node children]
  (if (map? node)
    (merge node (group-by :v/parentk children))
    children))

(defn- zipper [query-root-mapping ast]
  (z/zipper branch? (partial children query-root-mapping) make-node ast))

(defn- visit
  "Returns a map with 2 keys: :node contains the visited tree, :state contains the accumulated state"
  [query-root-mapping ast visitor-fns]
  (zv/visit (zipper query-root-mapping [{:node-type :query-root :children ast}]) {} visitor-fns))

(def ^:private document-sections [:type-system-definitions
                                  :fragment-definitions
                                  :operation-definitions])

(defn- root-query-name [document]                           ;; TODO use pre-existing schema functions
  (or (some->> document
               :type-system-definitions
               (filter #(= :schema-definition (:node-type %)))
               first
               :query-type
               :name) "Query"))

(defn query-root-mapping [document]                        ;; TODO use pre-existing schema functions
  (let [n (root-query-name document)]
    [n (some->> document
                :type-system-definitions
                (filter #(= (:type-name %) n))
                first
                :fields
                (map (juxt :field-name :type-name))
                (into {}))]))

(defn- merge-document [document]
  {:document (into {} (mapv (fn [[k v]] [k (-> v :node first :children)]) document))
   :state    (into {} (map (fn [[k v]] [k (-> v :state)]) document))})

;; Public API

(defn visit-document
  ([document root-query-mapping visitor-fns]
   (->> document-sections
        (reduce #(update %1 %2 (partial visit (or root-query-mapping {})) visitor-fns) document)
        merge-document))
  ([document visitor-fns]
   (visit-document document (query-root-mapping document) visitor-fns)))
