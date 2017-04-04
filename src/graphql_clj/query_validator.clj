(ns graphql-clj.query-validator
  (:require [graphql-clj.parser :as parser]
            [graphql-clj.schema-validator :as schema-validator]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]))


(defn- start-loc [m]
  {:line (:instaparse.gll/start-line m)
   :column (:instaparse.gll/start-column m)
   :index (:instaparse.gll/start-index m)})

(defn- end-loc [m]
  {:line (:instaparse.gll/end-line m)
   :column (:instaparse.gll/end-column m)
   :index (:instaparse.gll/end-index m)})
  
(defn- trace-element [n]
  (let [m (meta n)]
    {:source (format "fragment spread '%s'" (:name n))
     :start (start-loc m)
     :end (end-loc m)}))
    
(defn- add-trace [{:keys [trace]} err]
  (if (empty? trace)
    err
    (assoc err :trace (mapv trace-element trace))))

(defn- error [a node fmt & args]
  (let [m (meta node)]
    (update a :errors conj
            (add-trace a {:message (apply format fmt args)
                          :start (start-loc m)
                          :end (end-loc m)}))))

(defn- base-type [type]
  (case (:tag type)
    :basic-type (:name type)
    :list-type (recur (:inner-type type))))

(defn- type-string
  ([type] (.toString (type-string (StringBuilder.) type)))
  ([^StringBuilder buf type]
   (case (:tag type)
     :basic-type (.append buf (:name type))
     :list-type (-> buf
                    (.append \[)
                    (type-string (:inner-type type))
                    (.append \])))
   (if (:required type) (.append buf \!) buf)))

(defn- coersable-value [t v]
  (case (:tag t)
    :basic-type
    (case (:tag v)
      :boolean-value (= 'Boolean (:name t))
      :int-value     (#{'Int 'Float} (:name t))
      :float-value   (= 'Float (:name t))
      :string-value  (= 'String (:name t))
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

(defn- get-root [schema def]
  (get (:roots schema) (if (= :mutation-definition (:tag def)) :mutation :query)))


;; Compute the referenced fragment set from a given selection-set.
;; The result is a hash set of symbols of fragments referenced by the
;; selection set argument.
(defn- selection-set-fragment-refs [sset]
  (-> (fn accum [refs f]
        (case (:tag f)
          :selection-field refs
          :fragment-spread (conj! refs (:name f))
          :inline-fragment (reduce accum refs (:selection-set f))))
      (reduce (transient #{}) sset)
      (persistent!)))

;; Build a map from fragment name to referenced fragments. The key is
;; a symbol from the fragment name, the value is a set of referenced
;; fragments.
(defn- build-fragment-ref-graph [fmap]
  (reduce-kv #(assoc %1 %2 (selection-set-fragment-refs (:selection-set %3))) {} fmap))

;; Expand references in a graph.  g is a map from 'name to
;; #{'references}.  refs is a set of references that need to be
;; expanded.  E.g. g = {:a #{b c} :b #{c} :c #{}}, and refs = #{b c}, then
;; the result will be union of #{c} and #{}, thus #{c}.
(defn- expand-refs [g refs]
  (reduce set/union #{} (map g refs)))

(defn- check-fragment-cycles [a]
  (loop [a a g (build-fragment-ref-graph (:fragment-map a))]
    (if (empty? g)
      a
      ;; recurrance:
      ;; (1) cyclic reference is detected when a fragment references
      ;; itself after transitive dependencies are added.
      ;; (2) the graph is updated to remove all nodes with no
      ;; references remaining (thus cannot participate in a cyclic
      ;; reference), or when the error for the cyclic reference has
      ;; been recorded.  The map is then updated to add dependencies
      (recur (->> (filter #(contains? (% 1) (% 0)) g)
                  (reduce #(error %1 (%2 0) "fragment '%s' contains a cyclic reference" (%2 0)) a))
             (->> (remove (fn [[k v]] (or (contains? v k) (empty? v))) g)
                  (reduce (fn [g' [k v]]
                              (->> (map g v) ;; map a dependency to its frag reference sets
                                   (reduce set/union v) ;; union them together
                                   (filter g) ;; remove dependencies no longer in graph
                                   (into #{}) ;; put it back into a set, then assoc it to the updated graph
                                   (assoc g' k))) {}))))))

(declare check-selection-set)

(defn- check-argument [{:keys [arg-map] :as fdecl} {:keys [var-map] :as a} {:keys [name value] :as arg}]
  ;; (println "ARG:" fdecl arg var-map)
  (if-let [adecl (arg-map name)]
    (let [a (if (contains? (:arg-map a) name)
              (error a name "argument '%s' already has a value" name) ;; TODO: test this validation
              (update a :arg-map assoc name arg))]
      (case (:tag value)
        :variable-reference
        (if (nil? var-map) a ;; if var-map is missing, we're not checking vars
            (let [vname (:name value)]
              (if-let [vdecl (var-map vname)]
                (let [a (update a :var-use conj vname)]
                  (if (coersable-type (:type adecl) (effective-type vdecl))
                    a
                    (error a value "argument type mismatch: '%s' expects type '%s', argument '$%s' is type '%s'"
                           name (type-string (:type adecl)) vname (type-string (:type vdecl)))))
                (error a value "variable '$%s' is not defined" vname))))
        
        a))
    (error a value "field '%s' argument '%s' is not declared" (:name fdecl) name))) ;; TODO: test this validation

(defn- check-argument-assignment [{:keys [arg-map] :as a} argdecl]
  ;; (println argdecl)
  a)

(defn- check-argument-assignments [a argdecls]
  (reduce check-argument-assignment a argdecls))

;; Checks the arguments of a selection field.  This operates in two passes.
;; The first pass (via check-argument) builds an argument map, and checks referenced variables.
;; The second pass (via check-argument-assignment) checks that arguments are set or have default values.
(defn- check-arguments [a {argdecls :arguments :as fdecl} {args :arguments :as f}]
  (if (and (nil? argdecls) (nil? args))
    a ;; common case
    (-> (reduce (partial check-argument fdecl) (assoc a :arg-map {}) args)
        (check-argument-assignments argdecls)
        (dissoc :arg-map))))

(defn- check-field [tdef field-map a f]
  (case (:tag f)
    :selection-field
    (if-let [fdecl (field-map (:name f))]
      (check-selection-set (base-type (:type fdecl))
                           (check-arguments a fdecl f)
                           (assoc f :resolved-type (:type fdecl))
                           (fn [a f] (update a :vfields conj f)))
      (if (:alias f)
        (error a (:name f) "field '%s' (aliased as '%s') is not defined on type '%s'" (:name f) (:alias f) (:name tdef))
        (error a (:name f) "field '%s' is not defined on type '%s'" (:name f) (:name tdef))))

    :inline-fragment
    (let [on (get-in f [:on :name])]
      (if-let [ontype (get-in a [:schema :type-map on])]
        (if (contains? #{:type-definition :interface-definition :union-definition} (:tag ontype))
          a
          (error a on "inline fragment on non-composite type '%s'" on))
        (error a on "inline fragment on undefined type '%s'" on)))

    :fragment-spread
    (if-let [frag (get-in a [:fragment-map (:name f)])]
      (if (contains? (:trset a) (:name f)) ;; prevent cycles from causing stack overflow
        a ;; Could also do this: (error a (:name f) "fragment cycle detected")
        (check-selection-set (:name tdef)
                             (-> a (update :trace conj f) (update :trset conj (:name f)))
                             frag
                             (fn [a f] (-> a (update :trace pop) (update :trset disj (:name f))))))
      (if (empty? (:trace a)) ;; only warn about undefined fragments at top-level defs (when trace is empty)
        (error a (:name f) "fragment '%s' is not defined" (:name f))
        a))))

;; check-selection-set checks all members of a selection set and upon completion calls
;; (fassoc a (assoc def :selection-set <checked fields>))
(defn- check-selection-set [tname a def fassoc]
  (if-let [sset (:selection-set def)]
    (let [tdef (get-in a [:schema :type-map tname])
          field-map (:field-map tdef)
          prev-vfields (:vfields a)
          a (reduce (partial check-field tdef field-map) (assoc a :vfields []) sset)]
      (fassoc (assoc a :vfields prev-vfields) (assoc def :selection-set (:vfields a))))
    (fassoc a def)))

(defn- valid-variable-type? [schema tdecl]
  (#{:scalar-definition :enum-definition :input-definition} (:tag tdecl)))

(defn- map-var-decl [{:keys [schema] :as a} {:keys [name type default-value] :as v}]
  (let [btype (base-type type)
        tdecl (get-in schema [:type-map btype])]
    (cond
      (nil? tdecl)
      (error a type "variable '$%s' type '%s' is not defined" name btype)

      ;; TODO: can we give a more helpful message here--let the caller know that only scalar, enum, and input types are allowed.
      (not (valid-variable-type? schema tdecl))
      (error a v "variable '$%s' type '%s' is not a valid input type" name (type-string type))

      (contains? (:var-map a) name)
      (error a v "variable '$%s' is already declared" name)

      :else
      (or (and default-value
               (cond
                 (:required type)
                 (-> (error a v "variable '$%s' is required, and thus cannot have a default value" name)
                     (update :var-map assoc name (dissoc v :default-value)))

                 (= :variable-reference (:tag default-value))
                 (-> (error a default-value "variable default value must be constant")
                     (update :var-map assoc name (dissoc v :default-value)))

                 (not (coersable-value type default-value))
                 (-> (error a v "variable '$%s' has a default value that cannot be coersed to its declared type" name)
                     (update :var-map assoc name (dissoc v :default-value)))))

          (update a :var-map assoc name v)))))

(defn- map-var-decls [a def]
  (reduce map-var-decl (assoc a :var-map {} :var-use #{}) (:variable-definitions def)))

(defn- check-vars-used [a]
  (->> (keys (:var-map a))
       (remove (:var-use a))
       (reduce #(error %1 %2 "variable '$%s' is not used" %2) a)))

(defn- check-definition [a def]
  (case (:tag def)
    :selection-set
    (if (:anon a)
      (error a def "anonymous selection set is already declared")
      (check-selection-set (get-in a [:schema :roots :query]) a def
                           (fn [a def] (-> (assoc a :anon def)
                                           (update :query conj def)))))

    (:mutation :query-definition)
    (if (contains? (:def-map a) (:name def))
      ;; 5.1.1.1 operation name uniqueness
      (error a (:name def) "operation with name '%s' is already declared" (:name def))
      (check-selection-set (get-root (:schema a) def)
                           (map-var-decls a def)
                           def
                           (fn [a def] (-> (check-vars-used a)
                                           (dissoc :var-map :var-use)
                                           (update :def-map assoc (:name def) def)
                                           (update :query conj def)))))

    :fragment-definition
    (let [on (get-in def [:on :name])]
      (if-let [t (get-in a [:schema :type-map on])]
        (if (#{:type-definition :interface-definition :union-definition} (:tag t))
          (check-selection-set on a def
                               (fn [a def] (assoc-in a [:fragment-map (:name def)] def)))
          ;; 5.4.1.3 Fragments on composite types
          (error a on "fragment on non-composite type '%s'" on))
        (error a on "fragment on undefined type '%s'" on)))))

(defn- check-definitions [a]
  (reduce check-definition (assoc a :query []) (:query a)))

;; 5.1.2.1 lone anonymous operation
;; must be called after check-definition
(defn- check-lone-anonymous [a]
  (if (and (:anon a) (not (empty? (:def-map a))))
    (error a (:anon a) "anonymous selection set must be lone operation")
    a))

(defn- check-fragment [a def]
  (if (contains? (:fragment-map a) (:name def))
    (error a (:name def) "fragment '%s' is already declared" (:name def))
    (assoc-in a [:fragment-map (:name def)] def)))

(defn- build-fragment-map [a]
  (->> (:query a)
       (filter #(= :fragment-definition (:tag %)))
       (reduce check-fragment a)))

(defn- restructure [a]  [(:errors a) (:query a)])

(defn validate-query
  [schema query]
  (-> {:errors []
       :query query
       :schema schema
       :anon nil
       :fragment-map {}
       :trace '()
       :trset #{}
       :def-map {}}
      (build-fragment-map)
      (check-fragment-cycles)
      (check-definitions)
      (check-lone-anonymous)
      (restructure)))
