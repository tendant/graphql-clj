(ns graphql-clj.spec
  (:require [clojure.spec]
            [clojure.string :as str]
            [graphql-clj.visitor :as v]
            [clojure.walk :as walk]))

(def base-ns "graphql-clj")

(def delimiter "-")

(defn- append-pathlast [path s]
  (conj (butlast path) (str (last path) s)))

(defn- ?? [pred v] (or (nil? v) (pred v)))
(def int?? (partial ?? int?))
(def double?? (partial ?? double?))
(def string?? (partial ?? string?))
(def boolean?? (partial ?? boolean?))
(def id?? (partial ?? string?))

(def default-specs
  {"Int!"     int?     "Int"     int??
   "Float!"   double?  "Float"   double??
   "String!"  string?  "String"  string??
   "Boolean!" boolean? "Boolean" boolean??
   "ID!"      string?  "ID"      string??})

(def default-type-names (set (keys default-specs)))

(defn- add-required [n] (str n "!"))

(defn- spec-namespace [schema-hash path]
  (->> (butlast path) (mapv name) (into [base-ns schema-hash]) (str/join ".")))

(defn named-spec
  "Given a schema hash and a path for a type, return a registered spec identifier (namespaced keyword)"
  [schema-hash path]
  (cond (default-type-names (first path)) (keyword base-ns (first path))
        (keyword? path)                   path
        (and schema-hash (vector? path))  (keyword (spec-namespace schema-hash path) (name (last path)))))

(defn- type-names->args [type-names]
  (mapcat #(vector % %) type-names))

(defn- register-idempotent
  ([n pred]
   (eval (list 'clojure.spec/def n pred)))
  ([schema-hash path pred]
   (cond (keyword? (last path))                    (last path)
         (or (string? path) (string? (last path))) (register-idempotent (named-spec schema-hash path) pred)))
  ([schema-hash path pred required]
   (if required
     (register-idempotent schema-hash (append-pathlast path "!") pred)
     (register-idempotent schema-hash path #(or (nil? %) (pred %))))))

(doseq [[n pred] default-specs]
  (register-idempotent (keyword base-ns n) pred))

(defn path->name
  ([path] (when (> (count path) 0) (str/join delimiter path))) ;; Empty path vector = empty string = malformed keyword
  ([node-type path] (path->name (cons node-type path))))

(defn- field->spec [schema-hash {:keys [v/path]}]
  (named-spec schema-hash path))

(defn- to-keys [schema-hash fields]
  (list 'clojure.spec/keys
        :opt-un (map (partial field->spec schema-hash) (remove :required fields))
        :req-un (map (partial field->spec schema-hash) (filter :required fields))))

;; Spec for multimethod to add specs to relevant nodes

(defmulti spec-for
  (fn [_ {:keys [node-type type-name]}]
    (or node-type type-name)))

(defn- extension-type [schema-hash {:keys [v/path fields type-implements]}]
  (let [ext-spec (register-idempotent schema-hash (append-pathlast path (str delimiter "EXT")) (to-keys schema-hash fields))
        implements-specs (map (partial named-spec schema-hash) (or (:type-names type-implements) []))
        type-names (conj implements-specs ext-spec)]
    (register-idempotent schema-hash path (cons 'clojure.spec/or (type-names->args type-names)))))

(defn- base-type [schema-hash {:keys [v/path fields]}]
  (register-idempotent schema-hash path (to-keys schema-hash fields)))

(defmethod spec-for :type-definition [schema-hash {:keys [type-implements v/path] :as type-def}]
  (when (> (count path) 0)
    (if-not (empty? (:type-names type-implements))
      (extension-type schema-hash type-def)
      (base-type schema-hash type-def))))

(defmethod spec-for :variable-definition [schema-hash {:keys [v/path type-name]}]
  (register-idempotent schema-hash (into ["var"] path) (named-spec schema-hash [type-name])))

(defmethod spec-for :input-definition [schema-hash {:keys [type-name fields]}]
  (register-idempotent schema-hash [type-name] (to-keys schema-hash fields)))

(defmethod spec-for :union-definition [schema-hash {:keys [type-name type-names]}]
  (register-idempotent schema-hash [type-name] (cons 'clojure.spec/or (->> type-names
                                                                           (map (comp (partial named-spec schema-hash) vector))
                                                                           type-names->args))))

(defmethod spec-for :enum-definition [schema-hash {:keys [type-name fields]}]
  (register-idempotent schema-hash [type-name] (set (map :name fields))))

(defmethod spec-for :interface-definition [schema-hash {:keys [type-name fields]}]
  (register-idempotent schema-hash [type-name] (to-keys schema-hash fields)))

(defmethod spec-for :list [schema-hash {:keys [inner-type v/path] :as n}] ;; TODO ignores required
  #nu/tapd n
  (register-idempotent schema-hash path (list 'clojure.spec/coll-of (named-spec schema-hash [(:type-name inner-type)]))))

(defn- register-type-field [schema-hash path type-name]
  (let [full-type-name (path->name path)]                   ;; TODO do we want to be calling path->name here?
    (if (= full-type-name type-name)
      (named-spec schema-hash [type-name])
      (register-idempotent schema-hash path (named-spec schema-hash [type-name])))))

(defmethod spec-for :type-field [schema-hash {:keys [v/path type-name]}]
  (register-type-field schema-hash path type-name))

(defmethod spec-for :input-type-field [schema-hash {:keys [v/path type-name]}]
  (register-type-field schema-hash path type-name))

(defmethod spec-for :field [schema-hash {:keys [v/path]}]
  (named-spec schema-hash path))

(defmethod spec-for :argument [schema-hash {:keys [v/path]}]
  (named-spec schema-hash (into ["arg"] path)))

(defmethod spec-for :type-field-argument [schema-hash {:keys [v/path type-name]}]
  (register-idempotent schema-hash (into ["arg"] path) (named-spec schema-hash [type-name])))

(defmethod spec-for :default [_ _])

;; Visitors

(v/defmapvisitor keywordize :pre [n _]
  (cond
    (map? (:value n))         {:node (update n :value walk/keywordize-keys)}
    (map? (:default-value n)) {:node (update n :default-value walk/keywordize-keys)}
    :else                     nil))

(v/defmapvisitor add-spec :post [n s]
  (when-let [spec (spec-for (:schema-hash s) n)]
    {:node (assoc n :spec spec)}))
