(ns graphql-clj.spec
  (:require [clojure.spec :as s]
            [clojure.string :as str]
            [graphql-clj.visitor :as v]
            [clojure.walk :as walk]
            [graphql-clj.error :as ge]
            [zip.visit :as zv]
            [graphql-clj.type :as type])
  (:import [clojure.lang Compiler$CompilerException]))

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

(doseq [[n pred] default-specs] ;; Register specs for global base / default / scalar types
  (eval (list 'clojure.spec/def (keyword base-ns n) pred)))

(def base-type-names (set (keys type/default-types)))
(def default-type-names (set (keys default-specs)))

(defn- add-required [n] (str n "!"))

(defn- to-type-name [{:keys [type-name required]}] ;; TODO required is not supported for non-scalar types
  (if (and required (base-type-names type-name)) (add-required type-name) type-name))

(defn- spec-namespace [{:keys [schema-hash statement-hash]} path] ;; TODO make schema vs. statement hash decision upstream
  (->> (butlast path) (mapv name) (into [base-ns (or schema-hash statement-hash)]) (str/join ".")))

(defn named-spec
  "Given a schema hash and a path for a type, return a registered spec identifier (namespaced keyword)"
  [s path]
  (cond (default-type-names (first path)) (keyword base-ns (first path))
        (keyword? path)                   path
        (or (vector? path) (seq? path))   (keyword (spec-namespace s path) (name (last path)))
        :else                             (ge/throw-error "Unhandled named-spec case" {:path path})))

(defn- type-names->args [type-names]
  (mapcat #(vector % %) type-names))

(defn- recursive?
  "If pred is a keyword and also appears in the path of ancestor nodes, it's a recursive definition"
  [path pred]
  (and (keyword? pred) ((set path) (name pred))))

(defn- register-idempotent
  ([n pred] [n (list 'clojure.spec/def n pred)])
  ([s path pred]
   (cond (keyword? (last path)) [(last path)]
         (recursive? path pred) [pred]
         :else                  (register-idempotent (named-spec s path) pred))))

(defn- field->spec [s {:keys [v/path]}]
  (named-spec s path))

(defn- to-keys [s fields]
  (list 'clojure.spec/keys
        :opt-un (map (partial field->spec s) (remove :required fields))
        :req-un (map (partial field->spec s) (filter :required fields))))

;; Spec for multimethod to add specs to relevant nodes

(defmulti spec-for
  (fn [_ {:keys [node-type type-name]}]
    (or node-type type-name)))

(defn- extension-type [s {:keys [v/path fields type-implements]}]
  (let [ext-spec (register-idempotent s (append-pathlast path (str delimiter "EXT")) (to-keys s fields))
        implements-specs (map (partial named-spec s) (or (:type-names type-implements) []))
        type-names (conj implements-specs ext-spec)]
    (register-idempotent s path (cons 'clojure.spec/or (type-names->args type-names)))))


(defn- base-type [s {:keys [v/path fields]}]
  (register-idempotent s path (to-keys s fields)))

(defmethod spec-for :type-definition [s {:keys [type-implements v/path] :as type-def}]
  (when (> (count path) 0)
    (if-not (empty? (:type-names type-implements))
      (extension-type s type-def)
      (base-type s type-def))))

(defmethod spec-for :variable-definition [s {:keys [variable-name] :as n}]
  (register-idempotent (dissoc s :schema-hash) ["var" variable-name] (named-spec s [(to-type-name n)])))

(defmethod spec-for :variable-usage [s {:keys [variable-name]}]
  (named-spec (dissoc s :schema-hash) ["var" variable-name]))

(defmethod spec-for :input-definition [s {:keys [type-name fields]}]
  (register-idempotent s [type-name] (to-keys s fields)))

(defmethod spec-for :union-definition [s {:keys [type-name type-names]}]
  (register-idempotent s [type-name] (cons 'clojure.spec/or (->> type-names
                                                                 (map (comp (partial named-spec s) vector))
                                                                 type-names->args))))

;; TODO reverse order of s and n to be standard
(defmethod spec-for :enum-definition [s {:keys [type-name fields]}]
  (register-idempotent s [type-name] (set (map :name fields))))

;; TODO what if some fields on the associated type are required?
(defmethod spec-for :fragment-definition [s {:keys [v/path] :as n}]
  (register-idempotent (dissoc s :schema-hash) ["frag" (:name n)] (named-spec s path))) ;; alternatively, (to-keys s selection-set)

(defmethod spec-for :inline-fragment [s {:keys [v/path] :as n}]
  [(named-spec s [(last path)])])

(defmethod spec-for :fragment-spread [s n]
  [(named-spec (dissoc s :schema-hash) ["frag" (:name n)])])

(defmethod spec-for :interface-definition [s {:keys [type-name fields]}]
  (register-idempotent s [type-name] (to-keys s fields)))

(defn- coll-of
  "Recursively build up a nested collection"
  [s {:keys [inner-type required]}]
  (let [coll-list (list 'clojure.spec/coll-of (if (:type-name inner-type)
                                                (named-spec s [(to-type-name inner-type)])
                                                (coll-of s inner-type)))]
    (if required coll-list (list 'clojure.spec/nilable coll-list))))

(defmethod spec-for :list [s {:keys [v/path] :as n}]
  (register-idempotent s path (coll-of s n)))

(defn- register-type-field [s {:keys [v/path] :as n}]
  (if (and (= (count path) 1) (= (:type-name n) (first path)))
    [(named-spec s [(to-type-name n)])]
    (register-idempotent s path (named-spec s [(to-type-name n)]))))

(defmethod spec-for :type-field [s n]
  (register-type-field s n))

(defmethod spec-for :input-type-field [s n]
  (register-type-field s n))

(defmethod spec-for :field [s {:keys [v/path v/parent]}]
  [(named-spec s (if (= :inline-fragment (:node-type parent))
                   [(last (butlast path)) (last path)] ;; Ignore hierarchy for inline fragments
                   path))])

(defmethod spec-for :argument [s {:keys [v/path]}]
  [(named-spec s (into ["arg"] path))])

(defmethod spec-for :type-field-argument [s {:keys [v/path] :as n}]
  (register-idempotent s (into ["arg"] path) (named-spec s [(to-type-name n)])))

(defmethod spec-for :default [_ _])

(defmulti ^:private of-type (fn [n _] (:node-type n)))

(defmethod ^:private of-type :list [{:keys [inner-type]} s]
  (loop [it inner-type]
    (if (:type-name it)
      (named-spec s [(:type-name it)])
      (recur (:inner-type it)))))

(defmethod ^:private of-type :default [{:keys [spec]} _]
  spec)

;; Parent and base types

(defn get-type-node [spec s]
  "Given a spec, get the corresponding node from the AST"
  (get-in s [:spec-map spec]))

(defn get-base-type-node
  "Given a spec, get the node definition for the corresponding base type"
  [{:keys [spec]} s]
  (when-let [base-spec (s/get-spec spec)]
    (if (default-type-names (name base-spec))
      {:node-type :scalar :type-name (name base-spec)}
      (get-type-node base-spec s))))

(defn get-parent-type
  "Given a node and the global state, find the parent type"
  [{:keys [v/parent]} s]
  (if-let [base-parent (get-type-node (of-type parent s) s)]
    (of-type base-parent s)
    (recur parent s)))

;; Visitors

(defn- safe-eval [d]
  (assert (= (first d) 'clojure.spec/def))               ;; Protect against unexpected statement eval
  (try (eval d) (catch Compiler$CompilerException _ d))) ;; Squashing errors here to provide better error messages in validation

(def define-specs)
(zv/defvisitor define-specs :post [n s]
  (when (seq? n) ;; Top of the tree is a seq
    (some->>
      (some-> s :spec-defs)
      (mapv safe-eval)
      (filter (comp not keyword?))
      (mapv safe-eval)) ;; if eval failed the first time, try once more to help with order dependencies
    {:state (dissoc s :spec-defs)}))

(def add-spec)
(v/defmapvisitor add-spec :post [n s]
  (when-let [[spec-name spec-def] (spec-for s n)]
    (let [updated-n (-> n (assoc :spec spec-name))]
      (cond-> {:node (dissoc updated-n :v/parent)}
              spec-def (assoc :state (-> s
                                         (update :spec-defs #(conj (or % []) spec-def))
                                         (assoc-in [:spec-map spec-name] updated-n)))))))
