(ns graphql-clj.spec
  (:require [clojure.spec :as s]
            [clojure.string :as str]
            [graphql-clj.visitor :as v]
            [clojure.walk :as walk]
            [graphql-clj.error :as ge]
            [zip.visit :as zv]))

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
        (and schema-hash (vector? path))  (keyword (spec-namespace schema-hash path) (name (last path)))
        :else (ge/throw-error "Unhandled named-spec case" {:path path :schema-hash schema-hash})))

(defn- type-names->args [type-names]
  (mapcat #(vector % %) type-names))

(defn recursive? [path pred]
  (and (keyword? pred) ((set path) (name pred))))

(defn- register-idempotent! [n pred]
  (eval (list 'clojure.spec/def n pred)))

(defn- register-idempotent
  ([n pred] [n (list 'clojure.spec/def n pred)])
  ([schema-hash path pred]
   (recursive? path pred)
   (if (keyword? (last path))
     (last path)
     (register-idempotent (named-spec schema-hash path) pred)))
  ([schema-hash path pred required]
   (if required
     (register-idempotent schema-hash (append-pathlast path "!") pred)
     (register-idempotent schema-hash path #(or (nil? %) (pred %))))))

(doseq [[n pred] default-specs] ;; Register specs for global base / default / scalar types
  (register-idempotent! (keyword base-ns n) pred))

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

(defn- coll-of
  "Recursively build up a nested collection"
  [schema-hash inner-type]
  (list 'clojure.spec/coll-of (if (:type-name inner-type)
                                (named-spec schema-hash [(:type-name inner-type)])
                                (coll-of schema-hash (:inner-type inner-type)))))

(defmethod spec-for :list [schema-hash {:keys [inner-type v/path] :as n}] ;; TODO ignores required
  (register-idempotent schema-hash path (coll-of schema-hash inner-type)))

(defn- register-type-field [schema-hash path type-name]
  (if (and (= (count path) 1) (= type-name (first path)))
    (named-spec schema-hash [type-name])
    (register-idempotent schema-hash path (named-spec schema-hash [type-name]))))

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

(def define-specs)
(zv/defvisitor define-specs :post [n s]
  (when (seq? n)                                            ;; TODO handle recursive definitions here
    (doseq [d (some-> s :spec-defs reverse)] (eval d))      ;; TODO is this eval a potential security concern?  Has user input been entirely sanitized?
    {:state (dissoc s :spec-defs)}))

(def keywordize)
(v/defmapvisitor keywordize :pre [n _]
  (cond
    (map? (:value n))         {:node (update n :value walk/keywordize-keys)}
    (map? (:default-value n)) {:node (update n :default-value walk/keywordize-keys)}
    :else                     nil))

(def add-object-placeholder)
(v/defnodevisitor add-object-placeholder :pre :type-definition [n s])

(def add-spec)
(v/defmapvisitor add-spec :post [n s]
  (when-let [spec-def (spec-for (:schema-hash s) n)]
    (let [spec-name (if (vector? spec-def) (first spec-def) spec-def)
          node-update {:node (assoc n :spec spec-name)}]
      (if (vector? spec-def)
        (assoc node-update :state (update s :spec-defs conj (last spec-def)))
        node-update))))                                     ;; TODO dirty code
