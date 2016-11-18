(ns graphql-clj.validator.rules.variables-in-allowed-position
  "Variables passed to field arguments conform to type"
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]
            [clojure.spec :as s]
            [graphql-clj.spec :as spec]))

(defn- render-type [{:keys [type-name inner-type required]}]
  (if type-name
    (if required (spec/add-required type-name) type-name)
    (let [list-type-name (str "[" (render-type inner-type) "]")]
      (if required (spec/add-required list-type-name) list-type-name))))

(defn- implements [spec s]                                  ;; TODO needs to be recursive?
  (let [type-node (spec/get-type-node spec s)]
    (or (some->> type-node :implements :type-names (map name) set)      ;; Object implements interfaces
        (some->> type-node :type-names (map name) set)                  ;; Object union of other objects
        #{})))

(defn- variable-type-error [{:keys [variable-name] :as var-def} arg-def s]
  {:error (format "Variable '$%s' of type '%s' used in position expecting type '%s'."
                  variable-name (render-type var-def) (-> arg-def :spec (spec/get-type-node s) render-type))
   :loc   (ve/extract-loc (meta (:variable-name arg-def)))})

(defn- list-type? [spec] (s/valid? spec []))

(defn- required-type? [spec]
  (not (s/valid? spec nil)))

(defn subtype-of
  "A var type is allowed if it is the same or more strict (e.g. is
   a subtype of) than the expected type. It can be more strict if
   the variable type is non-null when the expected type is nullable.
   If both are list types, the variable item type can be more strict
   than the expected item type (contravariant)."
  [s maybe-sub-type super-type]
  (assert maybe-sub-type)
  (assert super-type)
  (cond (= maybe-sub-type super-type)                                                          true   ;; Equivalent type is a valid subtype
        (and (required-type? super-type) (not (required-type? maybe-sub-type)))                false  ;; If superType is non-null, maybeSubType must also be non-null.
        (not= (list-type? maybe-sub-type) (list-type? super-type))                             false  ;; If superType is not a list, maybeSubType must also be not a list, and vice versa.
        (= (spec/remove-required (namespace maybe-sub-type) (name maybe-sub-type)) super-type) true   ;; If superType is nullable, maybeSubType may be non-null or nullable.
        ((implements super-type s) (name maybe-sub-type))                                      true)) ;; If superType type is an abstract type, maybeSubType type may be a currently possible object type.

(defn- effective-type
  "If a variable definition has a default value, it's effectively non-null."
  [variable-type {:keys [default-value]}]
  (if (and default-value (not (s/valid? variable-type nil)))
    (spec/add-required (namespace variable-type) (name variable-type))
    variable-type))

(defnodevisitor variable-type-mismatch :post :argument
  [{:keys [spec variable-name] :as n} s]
  (when variable-name
    (let [arg-type (s/get-spec spec)
          var-spec (spec/spec-for {:node-type :variable-usage :variable-name variable-name} s)
          var-type (s/get-spec var-spec)
          var-def  (spec/get-type-node var-spec s)]
      (when-not (subtype-of s (effective-type var-type var-def) arg-type)
        {:state (ve/update-errors s (variable-type-error var-def n s))}))))

(def rules [variable-type-mismatch])
