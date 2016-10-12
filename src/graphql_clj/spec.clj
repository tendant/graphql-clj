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

(defn- spec-namespace [{:keys [schema-hash]} path]
  (->> (butlast path) (mapv name) (into [base-ns schema-hash]) (str/join ".")))

(defn named-spec
  "Given a schema hash and a path for a type, return a registered spec identifier (namespaced keyword)"
  [{:keys [schema-hash] :as s} path]
  (cond (default-type-names (first path)) (keyword base-ns (first path))
        (keyword? path)                   path
        (and schema-hash (vector? path))  (keyword (spec-namespace s path) (name (last path)))
        :else (ge/throw-error "Unhandled named-spec case" {:path path})))

(defn- type-names->args [type-names]
  (mapcat #(vector % %) type-names))

(defn recursive? [path pred]                                ;; TODO use somewhere?
  (and (keyword? pred) ((set path) (name pred))))

(defn- register-idempotent! [n pred]
  (eval (list 'clojure.spec/def n pred)))

(defn- register-idempotent
  ([n pred] [n (list 'clojure.spec/def n pred)])
  ([s path pred]
   (if (keyword? (last path))
     (last path)
     (register-idempotent (named-spec s path) pred)))
  ([s path pred required]
   (if required
     (register-idempotent s (append-pathlast path "!") pred)
     (register-idempotent s path #(or (nil? %) (pred %))))))

(doseq [[n pred] default-specs] ;; Register specs for global base / default / scalar types
  (register-idempotent! (keyword base-ns n) pred))

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

(defmethod spec-for :variable-definition [s {:keys [v/path type-name]}]
  (register-idempotent s (into ["var"] path) (named-spec s [type-name])))

(defmethod spec-for :input-definition [s {:keys [type-name fields]}]
  (register-idempotent s [type-name] (to-keys s fields)))

(defmethod spec-for :union-definition [s {:keys [type-name type-names]}]
  (register-idempotent s [type-name] (cons 'clojure.spec/or (->> type-names
                                                                           (map (comp (partial named-spec s) vector))
                                                                           type-names->args))))

(defmethod spec-for :enum-definition [s {:keys [type-name fields]}]
  (register-idempotent s [type-name] (set (map :name fields))))

(defmethod spec-for :interface-definition [s {:keys [type-name fields]}]
  (register-idempotent s [type-name] (to-keys s fields)))

(defn- coll-of
  "Recursively build up a nested collection"
  [s inner-type]
  (list 'clojure.spec/coll-of (if (:type-name inner-type)
                                (named-spec s [(:type-name inner-type)])
                                (coll-of s (:inner-type inner-type)))))

(defmethod spec-for :list [s {:keys [inner-type v/path] :as n}] ;; TODO ignores required
  (register-idempotent s path (coll-of s inner-type)))

(defn- register-type-field [s path type-name]
  (if (and (= (count path) 1) (= type-name (first path)))
    (named-spec s [type-name])
    (register-idempotent s path (named-spec s [type-name]))))

(defmethod spec-for :type-field [s {:keys [v/path type-name]}]
  (register-type-field s path type-name))

(defmethod spec-for :input-type-field [s {:keys [v/path type-name]}]
  (register-type-field s path type-name))

(defmethod spec-for :field [s {:keys [v/path]}]
  (named-spec s path))

(defmethod spec-for :argument [s {:keys [v/path]}]
  (named-spec s (into ["arg"] path)))

(defmethod spec-for :type-field-argument [s {:keys [v/path type-name]}]
  (register-idempotent s (into ["arg"] path) (named-spec s [type-name])))

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
  (when-let [spec-def (spec-for s n)]
    (let [spec-name (if (vector? spec-def) (first spec-def) spec-def)
          node-update {:node (assoc n :spec spec-name)}]
      (if (vector? spec-def)
        (assoc node-update :state (update s :spec-defs conj (last spec-def)))
        node-update))))                                     ;; TODO dirty code
