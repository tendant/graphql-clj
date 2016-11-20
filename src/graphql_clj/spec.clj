(ns graphql-clj.spec                                        ;; TODO move to graphql-clj.type
  (:require [clojure.spec :as s]
            [clojure.string :as str]
            [graphql-clj.visitor :as v]
            [graphql-clj.error :as ge]
            [zip.visit :as zv]
            [graphql-clj.visitor :refer [defnodevisitor]])
  (:import [clojure.lang Compiler$CompilerException]))

(def base-ns "graphql-clj")

(def delimiter "-")

(defn- append-pathlast [path s]
  (conj (butlast path) (str (last path) s)))

(defn boolean?* ;; TODO remove after clojure 1.9
  "From clojure.future: Return true if x is a Boolean"
  [x] (instance? Boolean x))

(defn int?* ;; TODO remove after clojure 1.9
  "From clojure.future: Return true if x is a fixed precision integer"
  [x] (or (instance? Long x)
          (instance? Integer x)
          (instance? Short x)
          (instance? Byte x)))

(defn double?* ;; TODO remove after clojure 1.9
  "From clojure.future: Return true if x is a Double"
  [x] (instance? Double x))

(defn- ??
  "Allow nil as well as a predicate"
  [pred v] (or (nil? v) (pred v)))

(def int??     (partial ?? int?*))
(def double??  (partial ?? double?*))
(def string??  (partial ?? string?))
(def boolean?? (partial ?? boolean?*))
(def id??      (partial ?? string?))

(def default-specs
  {"Int!"     int?*     "Int"     int??
   "Float!"   double?*  "Float"   double??
   "String!"  string?   "String"  string??
   "Boolean!" boolean?* "Boolean" boolean??
   "ID!"      string?   "ID"      string??})

(def default-spec-keywords ;; Register specs for global base / default / scalar types
  (set (mapv (fn [[n pred]] (eval (list 'clojure.spec/def (keyword base-ns n) pred))) default-specs)))

(def directive-specs
  {"include" {"if" "Boolean"}
   "skip"    {"if" "Boolean"}})

(defn directive-spec-name [directive-name arg-name]
  (keyword (str base-ns ".arg.@" (name directive-name)) (name arg-name)))

(doseq [[n args] directive-specs] ;; Register specs for supported directives
  (doseq [[arg spec-name] args]
    (eval (list 'clojure.spec/def (directive-spec-name n arg) (keyword base-ns spec-name)))))

(def default-types
  {"Int"     {:type-name "Int"     :kind :SCALAR}
   "Float"   {:type-name "Float"   :kind :SCALAR}
   "String"  {:type-name "String"  :kind :SCALAR}
   "Boolean" {:type-name "Boolean" :kind :SCALAR}
   "ID"      {:type-name "ID"      :kind :SCALAR}})

(def base-type-names (set (keys default-types)))
(def default-type-names (set (keys default-specs)))

(defn add-required
  ([name] (str name "!"))
  ([namespace name] (keyword namespace (add-required name))))

(defn remove-required
  ([name] (str/replace name #"\!$" ""))
  ([namespace name] (keyword namespace (remove-required name))))

(defn- to-type-name [{:keys [type-name required]}]
  (if (and required (base-type-names (name type-name))) (add-required type-name) (name type-name)))

(defn- spec-namespace [{:keys [schema-hash statement-hash]} path] ;; TODO make schema vs. statement hash decision upstream
  (->> (butlast path) (mapv name) (into [base-ns (or schema-hash statement-hash)]) (str/join ".")))

(defn named-spec
  "Given a schema hash and a path for a type, return a registered spec identifier (namespaced keyword)"
  [s path]
  (cond (some-> path first name default-type-names) (keyword base-ns (name (first path)))
        (keyword? path)                             path
        (or (vector? path) (seq? path))             (keyword (spec-namespace s path) (name (last path)))
        :else                                       (ge/throw-error "Unhandled named-spec case" {:path path})))

(defn- type-names->args [type-names]
  (mapcat #(vector % %) type-names))

(defn- recursive?
  "If pred is a keyword and also appears in the path of ancestor nodes, it's a recursive definition"
  [path pred]
  (and (keyword? pred) ((set path) (name pred))))

(defn- register-idempotent
  ([n pred meta] {:n n :d (list 'clojure.spec/def n pred) :m meta})
  ([s path pred meta]
   (cond (keyword? (last path)) {:n (last path) :m meta}
         (recursive? path pred) (register-idempotent (named-spec s (map str path)) pred (assoc meta :recursive true))
         :else                  (register-idempotent (named-spec s (map str path)) pred meta))))

(defn- field->spec [s {:keys [v/path]}]
  (named-spec s path))

(defn- to-keys [s fields]
  (list 'clojure.spec/keys
        :opt-un (map (partial field->spec s) (remove :required fields))
        :req-un (map (partial field->spec s) (filter :required fields))))

;; Parent and base types

(defmulti ^:private of-type (fn [n _] (:kind n)))

(defmethod ^:private of-type :LIST [{:keys [inner-type]} s]
  (loop [it inner-type]
    (if (:type-name it)
      (named-spec s [(:type-name it)])
      (recur (:inner-type it)))))

(defmethod ^:private of-type :default [{:keys [spec]} _]
  (let [base-spec (s/get-spec spec)]
    (if (keyword? base-spec) base-spec spec)))

(defn get-type-node
  "Given a spec, get the corresponding node from the AST"
  [spec s]
  (when (keyword? spec)
    (let [spec-name (name spec)]
      (if (default-type-names spec-name)
        (cond-> {:node-type :scalar :type-name spec-name :kind :SCALAR :spec spec} ;; TODO add to spec map eagerly
                (not (s/valid? spec nil)) (assoc :required true))
        (get-in s [:spec-map spec])))))

(defn get-base-type-node
  "Given a spec, get the node definition for the corresponding base type"
  [spec s]
  (let [base-spec* (s/get-spec spec)
        base-spec  (if (keyword? base-spec*) base-spec* spec)]
    (get-type-node base-spec s)))

(defn get-parent-type
  "Given a node and the global state, find the parent type"
  [{:keys [v/parent]} s]
  (let [parent-spec (of-type parent s)]
    (if-let [base-parent (get-type-node parent-spec s)]
      (of-type base-parent s)
      (if (and parent (or (:spec parent) (:kind parent)))
        (recur parent s)
        parent-spec))))

;; Spec for multimethod to add specs to relevant nodes

(defmulti spec-for
  (fn [{:keys [node-type type-name]} _]
    (or node-type type-name)))

(defn- extension-type [{:keys [v/path fields type-implements]} s]
  (let [ext-spec (register-idempotent s (append-pathlast path (str delimiter "EXT")) (to-keys s fields) {})
        implements-specs (map (partial named-spec s) (or (:type-names type-implements) []))
        type-names (conj implements-specs (:n ext-spec))]
    (register-idempotent s path (cons 'clojure.spec/or (type-names->args type-names)) {})))

(defn- base-type [{:keys [v/path fields]} s]
  (register-idempotent s path (to-keys s fields) {}))

(defmethod spec-for :type-definition [{:keys [type-implements v/path] :as type-def} s]
  (when (> (count path) 0)
    (if-not (empty? (:type-names type-implements))
      (extension-type type-def s)
      (base-type type-def s))))

(defn- coll-of
  "Recursively build up a nested collection"
  [{:keys [inner-type required]} s]
  (let [coll-list (list 'clojure.spec/coll-of (if (:type-name inner-type)
                                                (named-spec s [(to-type-name inner-type)])
                                                (coll-of inner-type s)))]
    (if required coll-list (list 'clojure.spec/nilable coll-list))))

(defmethod spec-for :variable-definition [{:keys [variable-name kind] :as n} s]
  (if (= :LIST kind)
    (register-idempotent (dissoc s :schema-hash) ["var" variable-name] (coll-of n s) {}) ;; TODO add metadata?
    (register-idempotent (dissoc s :schema-hash) ["var" variable-name] (named-spec s [(to-type-name n)]) {})))

(defmethod spec-for :variable-usage [{:keys [variable-name]} s]
  {:n (named-spec (dissoc s :schema-hash) ["var" variable-name]) :m {}})

(defn spec-for-var-usage [variable-name s]
  (:n (spec-for {:node-type :variable-usage :variable-name variable-name} s)))

(defmethod spec-for :input-definition [{:keys [type-name fields]} s]
  (register-idempotent s [type-name] (to-keys s fields) {}))

(defmethod spec-for :union-definition [{:keys [type-name type-names]} s]
  (register-idempotent s [type-name] (cons 'clojure.spec/or (->> type-names
                                                                 (map (comp (partial named-spec s) vector))
                                                                 type-names->args)) {}))

(defmethod spec-for :enum-definition [{:keys [type-name fields]} s]
  (register-idempotent s [type-name] (set (map :name fields)) {}))

(defmethod spec-for :fragment-definition [{:keys [v/path] :as n} s] ;; TODO fragment spec is equivalent to the type condition spec, when it should be a subset of those fields
  (let [base-spec (named-spec s path)
        base-type-node (get-type-node base-spec s)]
    (register-idempotent (dissoc s :schema-hash) ["frag" (:name n)] base-spec {:base-spec base-spec
                                                                               :kind (:kind base-type-node)})))

(defmethod spec-for :inline-fragment [{:keys [v/path]} s]
  (let [spec (named-spec s [(last path)])
        base-type-node (get-type-node spec s)]
    {:n spec :m {:base-spec spec :kind (:kind base-type-node)}}))

(defmethod spec-for :fragment-spread [n s]
  {:n (named-spec (dissoc s :schema-hash) ["frag" (:name n)]) :m {}})

(defmethod spec-for :interface-definition [{:keys [type-name fields]} s]
  (register-idempotent s [type-name] (to-keys s fields) {}))

(defmethod spec-for :list [{:keys [v/path] :as n} s]
  (register-idempotent s path (coll-of n s) {}))

(defn- register-type-field [{:keys [v/path] :as n} s]
  (if (and (= (count path) 1) (= (:type-name n) (first path)))
    {:n (named-spec s [(to-type-name n)]) :m {}}
    (register-idempotent s path (named-spec s [(to-type-name n)]) {})))

(defmethod spec-for :type-field [{:keys [v/path kind] :as n} s]
  (if (= :LIST kind)
    (register-idempotent s path (coll-of n s) {})           ;; TODO add metadata?
    (register-type-field n s)))

(defmethod spec-for :input-type-field [{:keys [v/path kind] :as n} s]
  (if (= :LIST kind)
    (register-idempotent s path (coll-of n s) {})           ;; TODO add metadata?
    (register-type-field n s)))

(defn- resolve-path [path]
  (if-let [t (some-> path second meta :type-name)]
    (into [t] (rest (rest path)))
    path))

(defn- get-parent-node [{:keys [v/parent]} s]
  (let [base-spec** (or (:base-spec parent) (:spec parent))
        base-spec* (s/get-spec base-spec**)
        base-spec (if (keyword? base-spec*) base-spec* base-spec**)]
    (get-type-node base-spec s)))

(defmethod spec-for :argument [{:keys [v/path v/parent]} s]
  (let [path (if (> (count path) 3) (resolve-path path) path)]
    (case (:node-type parent)
      :field     (let [n (named-spec s (into ["arg"] path))] {:n n :m {:base-spec (some-> n (get-base-type-node s) :spec)}})
      :directive {:n (directive-spec-name (-> parent :v/path last) (last path)) :m {}})))

(defmethod spec-for :type-field-argument [{:keys [v/path kind] :as n} s]
  (if (= :LIST kind)
    (register-idempotent s (into ["arg"] path) (coll-of n s) {}) ;; TODO add metadata?
    (register-idempotent s (into ["arg"] path) (named-spec s [(to-type-name n)]) {})))

;; Multimethod to decomplect getting base-spec and parent-type-name for fields

(defmulti spec-for-field
  (fn [{:keys [v/path v/parent]} parent-node s]
    (cond (#{:fragment-definition :inline-fragment} (:node-type parent))         :fragment-child
          (= :LIST (:kind parent-node))                                          :list-child
          (and (= (count path) 2) (or (= (first path) (:query-root-name s))
                                      (= (first path) (:mutation-root-name s)))) :root-field)))

(defmethod spec-for-field :fragment-child
  ;; Child fields of named and inline fragments jump out of the nesting path to the top level
  [{:keys [v/path v/parent]} _ s]
  (let [parent-type-name (name (:base-spec parent))
        spec             (named-spec s [parent-type-name (last path)])
        base-spec        (:spec (get-type-node spec s))]
    {:n spec :m {:base-spec base-spec :parent-type-name parent-type-name}}))

(defmethod spec-for-field :list-child
  ;; Child fields of list types need to be unwrapped to get to the parent and base types
  [{:keys [v/path]} parent-node s]
  (let [{:keys [type-name inner-type]} parent-node
        parent-type-name (or (:type-name inner-type) type-name) ;; TODO multiple levels of nesting?
        base-named-spec  (named-spec s [parent-type-name (last path)])
        base-spec        (:spec (get-type-node base-named-spec s))]
    {:n base-named-spec :m {:parent-type-name parent-type-name :base-spec base-spec}}))

(defmethod spec-for-field :root-field
  ;; Root fields smuggle their base types via metadata from visitor bootstrap
  [{:keys [v/path]} _ s]
  (let [base-spec (named-spec s [(last (resolve-path path))])] ;; Convert rootField => TypeName
    (register-idempotent s path base-spec {:parent-type-name (first path) :base-spec base-spec})))

(defmethod spec-for-field :default
  ;; By default, link this field to its parent object type path
  [{:keys [v/path]} parent-node s]
  (let [parent-type-name (:type-name parent-node)
        base-spec  (named-spec s (conj (:v/path parent-node) (last path)))]
    {:n base-spec :m {:parent-type-name parent-type-name :base-spec base-spec}}))

(defmethod spec-for :field [n s]
  (spec-for-field n (get-parent-node n s) s))

(defmethod spec-for :default [_ _])

(defn- safe-eval [recursive? {:keys [d m] :as spec-def}]
  (if (or recursive? (not (:recursive m)))
    (do
      (assert (= (first d) 'clojure.spec/def))                      ;; Protect against unexpected statement eval
      (try (eval d) (catch Compiler$CompilerException _ spec-def))) ;; Squashing errors here to provide better error messages in validation
    spec-def))

;; Visitors

(declare define-specs)
(zv/defvisitor define-specs :post [n s]
  (when (seq? n) ;; Top of the tree is a seq
    (some->>
      (some-> s :spec-defs)
      (mapv (partial safe-eval false)) ;; Don't register recursive types on the first pass
      (filter (comp not keyword?))
      (mapv (partial safe-eval true))) ;; If recursive (skipped) or eval failed the first time, try once more to help with order dependencies
    {:state (dissoc s :spec-defs)}))

(defn add-spec* [node s {:keys [n d m] :as spec-def}]
  (cond-> {:node (-> node (merge m) (assoc :spec n) (dissoc :v/parent))}
          d (assoc :state (update s :spec-defs #(conj (or % []) spec-def)))))

(declare add-spec-to-map)
(v/defmapvisitor add-spec-to-map :post [{:keys [spec] :as n} s]
  (when spec {:state (update-in s [:spec-map spec] ;; Fragment spreads have the same name as fragment definitions, and can be processed earlier
                                #(or % (when-not (= :fragment-spread (:node-type n)) n)))}))

(declare add-spec)
(v/defmapvisitor add-spec :post [n s]
  (when-let [spec-def (spec-for n s)] (add-spec* n s spec-def)))

(declare add-spec-pre)
(v/defmapvisitor add-spec-pre :pre [n s]
  (when-let [spec-def (spec-for n s)]
    (let [base-spec (some-> spec-def :m :base-spec)]
      (cond-> (add-spec* n s spec-def)
              base-spec (assoc-in [:node :base-spec] base-spec)))))

(declare fix-lists)
(defnodevisitor fix-lists :pre :list
  [{:keys [variable-name field-name argument-name v/parent v/parentk] :as n} s]
  (let [parent-type (:node-type parent)]
    (cond (and field-name (= :input-definition parent-type))
          {:node (assoc n :node-type :input-type-field)}

          (and argument-name (= :type-field parent-type))
          {:node (assoc n :node-type :type-field-argument)}

          (and field-name (= :type-definition parent-type))
          {:node (assoc n :node-type :type-field)}

          (and field-name (= :interface-definition parent-type))
          {:node (assoc n :node-type :type-field)}

          (and variable-name (= :variable-definitions parentk))
          {:node (assoc n :node-type :variable-definition)})))
