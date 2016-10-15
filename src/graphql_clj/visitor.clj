(ns graphql-clj.visitor
  (:require [clojure.zip :as z]
            [zip.visit :as zv]
            [graphql-clj.type :as type]))

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
   :query-root             #{:children}
   :fragment-definition    #{:selection-set}})

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
   :input-definition     [:type-name]
   :fragment-definition  [(comp :type-name :type-condition)]
   :fragment-spread      [:name]})

;; Zipper

(def ^:private keys-with-children
  (reduce (fn [acc [_ v]] (into acc v)) #{} node-type->children))

(def ^:private has-children?
  (apply some-fn keys-with-children))

(def ^:private node-type->label-key-fn
  (->> (map (fn [[k v]] [k (apply some-fn v)]) node-type->label-key) (into {})))

(defn- branch? [node]
  (or (vector? node) (and (map? node) (has-children? node))))

(defn- node->label [{:keys [query-root-fields query-root-name]} {:keys [node-type] :as node}]
  (when-let [label-fn (get node-type->label-key-fn node-type)]
    (let [label (label-fn node)
          label (get query-root-fields label label)]
      (when (not= label query-root-name) label))))

(defn- parent-path [initial-state parent]
  (or (:v/path parent) (if-let [label (node->label initial-state parent)] [label] [])))

(defn conj-child-path [initial-state child parent-path]
  (let [child-label (node->label initial-state child)]
    (if (and child-label (not= child-label (first parent-path)))
      (conj parent-path child-label)
      parent-path)))

(def relevant-parent-keys #{:node-type :inner-type :type-name :spec :v/parent :v/path})

(defn- add-path [initial-state parentk parent child]
  (assoc child :v/parent (select-keys parent relevant-parent-keys) ;; TODO is this slow?
               :v/parentk parentk
               :v/path (->> (parent-path initial-state parent)
                            (conj-child-path initial-state child))))

(defn- children-w-path [initial-state node f]
  (->> (f node) (map (partial add-path initial-state f node))))

(defn- children [initial-state node]
  (cond (vector? node) node
        (map? node) (->> (get node-type->children (:node-type node))
                         (mapcat (partial children-w-path initial-state node)))))

(defn- make-node [node children]
  (if (map? node)
    (merge node (group-by :v/parentk children))
    children))

(defn- zipper [document initial-state]
  (z/zipper branch? (partial children initial-state) make-node document))

(defn- visit
  "Returns a map with 2 keys: :node contains the visited tree, :state contains the accumulated state"
  [document initial-state visitor-fns]
  (zv/visit (zipper [{:node-type :query-root :children document}] initial-state) initial-state visitor-fns))

(def ^:private document-sections [:type-system-definitions
                                  :fragment-definitions
                                  :operation-definitions])

(defn- nodes->map [document]
  (->> document
       (mapv (fn [[k v]] [k (-> v :node first :children)]))
       (into {})))

(defn- update-document [acc section visitor-fns]
  (let [result (update-in acc [:document section] visit (:state acc) visitor-fns)]
    (-> result
        (assoc :state (-> result :document section :state))
        (update-in [:document section] dissoc :state))))

;; Public API

(defmacro mapvisitor
  [type bindings & body]
  `(fn [d# n# s#]
     (when (and (= ~type d#) (map? n#))
       (loop [n*# n# s*# s#] (let [~bindings [n*# s*#]] ~@body)))))

(defmacro defmapvisitor
  [sym type bindings & body]
  `(def ~sym (mapvisitor ~type ~bindings ~@body)))

(defmacro nodevisitor
  [type node-type bindings & body]
  `(fn [d# n# s#]
     (when (and (= ~type d#) (map? n#) (= ~node-type (:node-type n#)))
       (loop [n*# n# s*# s#]
         (let [~bindings [n*# s*#]]
           ~@body)))))

(defmacro defnodevisitor
  [sym type node-type bindings & body]
  `(def ~sym (nodevisitor ~type ~node-type ~bindings ~@body)))

(defn initial-state [schema]
  (let [query-root-name (type/query-root-name schema)]
    {:query-root-name   query-root-name
     :query-root-fields (type/query-root-fields query-root-name schema)
     :schema-hash       (hash schema)}))

(defn visit-document
  ([document visitor-fns] (visit-document document (initial-state document) visitor-fns))
  ([document initial-state visitor-fns]
   (-> (reduce #(update-document %1 %2 visitor-fns) {:document document :state initial-state} document-sections)
       (update :document nodes->map))))
