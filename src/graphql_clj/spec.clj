(ns graphql-clj.spec
  (:require [clojure.spec :as s]
            [clojure.string :as str]
            [zip.visit :as zv]))

(defn- named-spec
  "Given an unqualified string, return a registered spec identifier (namespaced keyword)"
  [type-name]
  (keyword "graphql-clj.spec" type-name))                   ;; TODO missing namespace per operation / schema

(defn- type-names->args [type-names]
  (mapcat #(vector % %) type-names))

(defn- register-idempotent
  ([spec-name pred]
   (cond (keyword? spec-name) spec-name
         (string? spec-name)  (eval (list 'clojure.spec/def (named-spec spec-name) pred))
         :else                pred))
  ([spec-name pred required]
   (if required
     (register-idempotent spec-name pred)
     (register-idempotent (if (keyword? pred) (str (name pred) "*") spec-name) (s/nilable pred))))) ;; TODO inconsistent treatment for scalars with trailing * missing

(defn path->name
  ([path] (when (> (count path) 0) (str/join "-" path))) ;; Empty path vector = empty string = malformed keyword
  ([node-type path] (path->name (cons node-type path))))

(defn- field->spec [{:keys [v/path]}] ;; TODO ignores required, rethink approach and notation for required fields
  (named-spec (path->name path)))

(defn- to-keys [fields]
  (list 'clojure.spec/keys :opt (map field->spec fields)))

(defmulti spec-for
  (fn [{:keys [node-type type-name]}]
    (or node-type type-name)))

;; TODO do programmatically
(register-idempotent "Int" int? false)
(register-idempotent "Int" int? true)
(register-idempotent "Float" double? false)
(register-idempotent "Float" double? true)
(register-idempotent "String" string? false)
(register-idempotent "String" string? true)
(register-idempotent "Boolean" boolean? false)
(register-idempotent "Boolean" boolean? true)
(register-idempotent "ID" string? false)
(register-idempotent "ID" string? true)

(defn- extension-type [{:keys [v/path fields type-implements]}]
  (let [full-type-name (path->name path)
        ext-spec (register-idempotent (str full-type-name "-EXT") (to-keys fields))
        implements-specs (map named-spec (or (:type-names type-implements) []))
        type-names (conj implements-specs ext-spec)]
    (register-idempotent (path->name path) (cons 'clojure.spec/or (type-names->args type-names)))))

(defn- base-type [{:keys [v/path fields]}]
  (register-idempotent (path->name path) (to-keys fields))) ;; TODO: on a pre-order traversal fields don't have paths

(defmethod spec-for :type-definition [{:keys [type-implements v/path] :as type-def}]
  (when (> (count path) 0)
    (if-not (empty? (:type-names type-implements))
      (extension-type type-def)
      (base-type type-def))))

(defmethod spec-for :variable-definition [{:keys [variable-name type-name]}]
  (register-idempotent (str "var-" variable-name) (named-spec type-name)))

(defmethod spec-for :input-definition [{:keys [type-name fields]}]
  (register-idempotent type-name (to-keys fields)))

(defmethod spec-for :union-definition [{:keys [type-name type-names]}]
  (register-idempotent type-name (cons 'clojure.spec/or (->> type-names (map named-spec) type-names->args))))

(defmethod spec-for :enum-definition [{:keys [type-name fields]}]
  (register-idempotent type-name (set (map :name fields))))

(defmethod spec-for :interface-definition [{:keys [type-name fields]}]
  (register-idempotent type-name (to-keys fields)))

(defmethod spec-for :directive-definition [type-def])                  ;; TODO predicate spec
(defmethod spec-for :schema-definition [type-def])                     ;; TODO predicate spec

(defmethod spec-for :list [{:keys [inner-type v/path]}] ;; TODO ignores required
  (register-idempotent (path->name path) (list 'clojure.spec/coll-of (named-spec (:type-name inner-type)))))

(defn- register-type-field [path type-name]
  (let [full-type-name (path->name path)]
    (if (= full-type-name type-name)
      (named-spec full-type-name)
      (register-idempotent (path->name path) (named-spec type-name)))))

(defmethod spec-for :type-field [{:keys [v/path type-name]}]
  (register-type-field path type-name))

(defmethod spec-for :input-type-field [{:keys [v/path type-name]}]
  (register-type-field path type-name))

(defmethod spec-for :field [{:keys [v/path]}]
  (named-spec (path->name path)))

(defmethod spec-for :argument [{:keys [v/path]}]
  (named-spec (path->name "arg" path)))

(defmethod spec-for :type-field-argument [{:keys [v/path type-name]}]
  (register-idempotent (path->name "arg" path) (named-spec type-name)))

(defmethod spec-for :default [_])

(zv/defvisitor add-spec :post [n s]
  (when-let [spec (and (map? n) (spec-for n))]
    {:node (assoc n :spec spec)}))
