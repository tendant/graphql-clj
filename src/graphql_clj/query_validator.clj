(ns graphql-clj.query-validator
  (:require [graphql-clj.parser :as parser]
            [graphql-clj.schema-validator :as schema-validator]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]))

(def ^:private ^:dynamic *schema*)
(def ^:private ^:dynamic *fragment-map*)
(def ^:private ^:dynamic *trace* {:stack '() :set #{}})
(def ^:private ^:dynamic *var-use*)
  
(defn- start-loc [m] (:start m))
(defn- end-loc [m] (:end m))
  
(defn- trace-element [n]
  (let [m (meta n)]
    {:source (format "fragment spread '%s'" (:name n))
     :start (start-loc m)
     :end (end-loc m)}))

(defn- err [errors add node fmt & args]
  (if-not add
    errors
    (let [m (meta node)
          e {:message (apply format fmt args)
             :start (start-loc m)
             :end (end-loc m)}]
      (conj errors (if (empty? (:stack *trace*)) e (assoc e :trace (mapv trace-element (:stack *trace*))))))))

;; (defn- make-errors []
;;   (-> (fn err
;;         ([errors] errors)
;;         ([errors node fmt & args]
;;          (let [errors (conj errors {:message (apply format fmt args)})]
;;            (partial err errors))))
;;       (partial [])))    

(defn- base-type [type]
  (case (:tag type)
    :basic-type (:name type)
    :list-type (recur (:inner-type type))))

(defn- type-string
  ([type] (.toString (type-string (StringBuilder.) type)))
  ([^StringBuilder buf type]
   (case (:tag type)
     :basic-type (.append buf (:name type))
     :list-type (-> buf (.append \[) (type-string (:inner-type type)) (.append \])))
   (if (:required type) (.append buf \!) buf)))

(def ^:private tag-image
  {:mutation "mutation"
   :query-definition "query"})

(defn- coersable-value [t v]
  (case (:tag t)
    :basic-type
    (case (:tag v)
      :boolean-value (= 'Boolean (:name t))
      :int-value     (or (= 'Int (:name t)) (= 'Float (:name t)))
      :float-value   (= 'Float (:name t))
      :string-value  (or (= 'String (:name t))
                         (not (#{'Boolean 'Int 'Float 'String} (:name t))) ; FIXME: work around for enum values
                         )
      :null-value    (not (:required t))
      :enum-value    true ;; TODO
      :list-value    false
      :object-value  true)

    :list-type
    (case (:tag v)
      ;; TODO: provide error and location for each mismatch
      :list-value (every? (partial coersable-value (:inner-type t)) (:values v))
      :null-value (not (:required t))
      false)))

(def ^:private composite-type? #{:type-definition :interface-definition :union-definition})

(def ^:private primitive-tag-to-type-name-map
  {:boolean-value 'Boolean
   :int-value 'Int
   :float-value 'Float
   :string-value 'String})

(defn- coersable-type [t s]
  (and (or (not (:required t)) (:required s))
       (case (:tag t)
         :basic-type
         (and (= :basic-type (:tag s))
              (or (= (:name t) (:name s))
                  (and (= 'Float (:name t)) (= 'Int (:name s)))))
         :list-type
         (and (= :list-type (:tag s))
              (recur (:inner-type t) (:inner-type s))))))

;; Returns the effective type of a variable.  This is essentially the
;; type of the variable with a '!' appended if there is a non-null
;; default value.
(defn- effective-type [{:keys [default-value type]}]
  (if (and default-value (not= :null-value (:tag default-value)))
    (assoc type :required true)
    type))

(defn- check-argument [var-map fdecl declaration instantiated
                       [errors assigned-set]
                       {:keys [name value] :as arg}]
  [(if-let [adecl (get-in fdecl [:arg-map name])]
     (let [errors (cond-> errors
                    (assigned-set name)
                    (err declaration name "argument '%s' already has a value" name))]
       (case (:tag value)
         :variable-reference
         (let [vname (:name value)
               vdecl (var-map vname)]
           (if (nil? vdecl)
             (err errors instantiated value "variable '$%s' is not defined" vname)
             (do
               (swap! *var-use* conj vname)
               (if (= :error vdecl)
                 errors ;; there's a problem with this variable, do not check its usage
                 (cond-> errors
                   (not (coersable-type (:type adecl) (effective-type vdecl)))
                   (err instantiated value "argument type mismatch: '%s' expects type '%s', argument '$%s' is type '%s'"
                        name (type-string (:type adecl)) vname (type-string (:type vdecl))))))))
         
         (:boolean-value :int-value :float-value :string-value)
         (if (coersable-value (:type adecl) value)
           errors
           (err errors declaration value "argument type mismatch: '%s' expects type '%s', argument is type '%s'"
                name (type-string (:type adecl)) (primitive-tag-to-type-name-map (:tag value))))

         :null-value
         (if (get-in adecl [:type :required])
           (err errors declaration value "required argument '%s' is null" name)
           errors)

         :object-value
         (if-not (= :basic-type (get-in adecl [:type :tag]))
           (err errors declaration value "argument type mismatch: '%s' expects type '%s', argument is type '%s'"
                name (type-string (:type adecl)) (primitive-tag-to-type-name-map (:tag value)))
           ;;otype (get-in *schema* [:type-map (:type adecl)])
           ;;(println "OTYPE:" otype)
           errors)

         ;; TODO: add validation on list and enum literals
         (:list-value :enum-value)
         errors))
     (err errors declaration value "argument '%s' is not defined on '%s'" name (:name fdecl)))
   (conj assigned-set name)])

(defn- check-required-assignments [[errors assigned-set] f argdecls declaration]
  (reduce (fn [errors {:keys [name type]}]
            (cond-> errors
              (and (:required type) (not (assigned-set name)))
              (err declaration f "required argument '%s' is missing" name)))
          errors
          argdecls))

(defn- check-arguments [errors
                        var-map
                        {argdecls :arguments :as fdecl}
                        {args :arguments :as f}
                        declaration instantiated]
  (if (and (nil? argdecls) (nil? args))
    errors
    (-> (reduce (partial check-argument var-map fdecl declaration instantiated) [errors #{}] args)
        (check-required-assignments f argdecls declaration))))

(defn- fragment-cycle-string [fname]
  ;; the stack trace is in a list in reverse order and may include containing elements
  ;; this for reverses the order and stops at the specified fragment name.
  (->> (into '("...") (for [e (:stack *trace*) :let [n (:name e)] :while (not= n fname)] n))
       (cons fname)
       (str/join " -> ")))

(defn- merge-in-selection-set [sset vfrag]
  (into sset (:selection-set vfrag)))

;; check the :selection-set member of a decl and return decl with a
;; validated :selection-set
(defn- check-selection-set [errors var-map tname decl declaration instantiated]
  ;; Checks on selection fields generate errors based upon their context.
  ;; We break the context into two booleans:
  ;;   declaration = top-level declaration of a selection-set.  declarations checks are
  ;;     checked once per source appearance.
  ;;   instantiated = queries and mutations, as well as recursively included fragments.
  (if (= :error decl)
    [errors decl] ;; declaration already marked as error, skip
    (let [fmap (get-in *schema* [:type-map tname :field-map])
          r (fn [[errors sset] f]
              (case (:tag f)
                :selection-field
                (let [fname (:name f)]
                  (if-let [fdecl (fmap fname)]
                    (let [errors (check-arguments errors var-map fdecl f declaration instantiated)
                          [errors vf] (if (:selection-set f)
                                        (check-selection-set errors var-map (base-type (:type fdecl)) f declaration instantiated)
                                        [errors f])]
                      [errors (conj sset (assoc vf :resolved-type (:type fdecl)))])
                    [(if-let [alias (:alias f)]
                       (err errors declaration fname "field '%s' (aliased as '%s') is not defined on type '%s'" fname alias tname)
                       (err errors declaration fname "field '%s' is not defined on type '%s'" fname tname))
                     sset]))
          
                :inline-fragment
                (let [on (get-in f [:on :name])
                      ontype (get-in *schema* [:type-map on])]
                  (if (composite-type? (:tag ontype))
                    (let [[errors vf] (check-selection-set errors var-map on f declaration instantiated)]
                      ;; [errors (merge-in-selection-set sset vf)]
                      [errors (conj sset vf)])
                    [(if ontype
                       (err errors declaration on "inline fragment on non-composite type '%s'" on)
                       (err errors declaration on "inline fragment on undefined type '%s'" on))
                     sset]))
          
                :fragment-spread
                (let [fname (:name f)]
                  (if (get-in *trace* [:set fname])
                    [(err errors true fname "fragment cycle detected: %s" (fragment-cycle-string fname)) sset]
                    (if-let [frag (*fragment-map* (:name f))]
                      (if instantiated
                        (binding [*trace* (-> *trace* (update :stack conj f) (update :set conj fname))]
                          (let [[errors vf] (check-selection-set errors var-map tname frag false true)]
                            [errors (merge-in-selection-set sset vf)])) ;; use [errors (conj sset vf)] to return inlined fragments
                        [errors sset])
                      ;; only warn about undefined fragments at top-level
                      ;; decls (detected by an empty trace).  Otherwise every
                      ;; included fragment would cause this warning.
                      ;;(if (empty? (:stack *trace*))
                      [(err errors declaration fname "fragment '%s' is not defined" fname) sset])))))
          [errors sset] (reduce r [errors []] (:selection-set decl))]
      [errors (assoc decl :selection-set sset)])))
  
(def ^:private valid-variable-type? #{:scalar-definition :enum-definition :input-definition})

(defn- check-variable-type [errors {:keys [name type] :as v} tdecl]
  (if (nil? tdecl)
    (err errors true type "variable '$%s' type '%s' is not defined" name (type-string type))
    (if-not (valid-variable-type? (:tag tdecl))
      (err errors true v "variable '$%s' type '%s' is not a valid input type" name (type-string type))
      errors)))

(defn- check-variable-defaults [errors {:keys [name type default-value] :as v}]
  (let [errors (cond-> errors
                 (:required type)
                 (err true v "variable '$%s' is required, and thus cannot have a default value" name))]
    (if (= :variable-reference (:tag default-value))
      (err errors true default-value "variable default value must be constant")
      (cond-> errors
        (not (coersable-value type default-value))
        (err true v "variable '$%s' has a default value that cannot be coersed to its declared type" name)))))

(defn- map-var-decl [[errors var-map] {:keys [name type default-value] :as v}]
  (let [updated-errors
        (cond-> (check-variable-type errors v (get-in *schema* [:type-map (base-type type)]))
          (contains? var-map name)
          (err true v "variable '$%s' is already declared" name)

          default-value
          (check-variable-defaults v))]
    [updated-errors (assoc var-map name (if (identical? errors updated-errors) v :error))]))

(defn- check-vars-used [errors var-map var-use]
  (->> (keys var-map)
       (remove var-use)
       (reduce #(err %1 true %2 "variable '$%s' is not used" %2) errors)))

(defn- check-definition [[errors vquery dmap] decl]
  (case (:tag decl)
    :selection-set
    (let [[errors vdecl]
          (-> (cond-> errors (dmap nil) (err true decl "anonymous selection set is already declared"))
              (check-selection-set {} (get-in *schema* [:roots :query]) decl true true))]
      [errors (conj vquery vdecl) (assoc dmap nil vdecl)])
        
    (:mutation :query-definition)
    (let [name (:name decl)
          root (case (:tag decl)
                 :mutation (get-in *schema* [:roots :mutation])
                 :query-definition (get-in *schema* [:roots :query]))
          errors (cond-> errors (dmap name) (err true name "operation with name '%s' is already declared" name))
          [errors var-map] (reduce map-var-decl [errors {}] (:variable-definitions decl))]
      (if (nil? root)
        [(err errors true decl "schema does not define a root '%s' type" (tag-image (:tag decl))) vquery dmap]
        (let [var-use (atom #{})
              ;; TODO: do not call check-selection-set when (nil? root)
              [errors vdecl] (binding [*var-use* var-use]
                               (check-selection-set errors var-map root decl true true))
              errors (check-vars-used errors var-map @var-use)]
          [errors (conj vquery vdecl) (assoc dmap name vdecl)])))

    :fragment-definition
    (let [on (get-in decl [:on :name])
          ontype (get-in *schema* [:type-map on])]
      (if (composite-type? (:tag ontype))
        (let [[errors v] (check-selection-set errors {} on decl true false)]
          [errors vquery dmap])
        ;; skip non-composite fragments, these were already checked
        ;; when building the fragment map.
        [errors vquery dmap]))
    (throw (ex-info (format "Unhandled definition tag: %s" decl) {:tag (:tag decl)
                                                                  :definition decl}))))

(defn- check-fragments [errors query]
  (->> (filter #(= :fragment-definition (:tag %)) query)
       (reduce (fn [[errors fragmap] {:keys [name on] :as f}]
                 (assert fragmap)
                 (if (fragmap name)
                   [(err errors true name "fragment '%s' is already declared" name) (assoc fragmap name :error)]
                   (let [t (get (*schema* :type-map) (on :name))]
                     (if (composite-type? (:tag t))
                       [errors (assoc fragmap name f)]
                       [(err errors true (on :name) (if (nil? t)
                                                      "fragment on undefined type '%s'"
                                                      "fragment on non-composite type '%s'") (on :name))
                        (assoc fragmap name :error)]))))
               [[] {}])))

(defn- check-lone-anonymous [[errors vquery dmap]]
  (let [anon (dmap nil)]
    [(cond-> errors
       (and anon (< 1 (count dmap)))
       (err true anon "anonymous selection set must be lone operation"))
     vquery]))

(defn- validate-query*
  [schema query]
  (binding [*schema* schema]
    (let [[errors fragmap] (check-fragments [] query)]
      (binding [*fragment-map* fragmap]
        (-> (reduce check-definition [errors [] {}] query)
            (check-lone-anonymous))))))

(defn validate-query
  [schema query]
  (let [parsed-query (if (string? query)
                       (parser/parse-query-document query)
                       query)]
    (validate-query* schema parsed-query)))
        
