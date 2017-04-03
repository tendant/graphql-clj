(ns graphql-clj.query-validator
  (:require [graphql-clj.parser :as parser]
            [graphql-clj.schema-validator :as schema-validator]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]))

;; TODO: share with schema validator
(defn- err [errors node fmt & args]
  (let [m (meta node)]
    (conj errors {:message (apply format fmt args)
                  :start {:line (:instaparse.gll/start-line m)
                          :column (:instaparse.gll/start-column m)
                          :index (:instaparse.gll/start-index m)}
                  :end {:line (:instaparse.gll/end-line m)
                        :column (:instaparse.gll/end-column m)
                        :index (:instaparse.gll/end-index m)}})))

(defn- base-type [type]
  (case (:tag type)
    :basic-type (:name type)
    :list-type (recur (:inner-type type))))

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

(defn- check-fragment-cycles [errors fmap]
  (loop [errors errors g (build-fragment-ref-graph fmap)]
    (if (empty? g)
      errors
      ;; recurrance:
      ;; (1) cyclic reference is detected when a fragment references
      ;; itself after transitive dependencies are added.
      ;; (2) the graph is updated to remove all nodes with no
      ;; references remaining (thus cannot participate in a cyclic
      ;; reference), or when the error for the cyclic reference has
      ;; been recorded.  The map is then updated to add dependencies
      (recur (->> (filter #(contains? (% 1) (% 0)) g)
                  (reduce #(err %1 (%2 0) "fragment '%s' contains a cyclic reference" (%2 0)) errors))
             (->> (remove (fn [[k v]] (or (contains? v k) (empty? v))) g)
                  (reduce (fn [g' [k v]]
                              (->> (map g v) ;; map a dependency to its frag reference sets
                                   (reduce set/union v) ;; union them together
                                   (filter g) ;; remove dependencies no longer in graph
                                   (into #{}) ;; put it back into a set, then assoc it to the updated graph
                                   (assoc g' k))) {}))))))

(defn- check-fragment-cycles [a]
  (loop [errors (:errors a) g (build-fragment-ref-graph (:fragment-map a))]
    (if (empty? g)
      (assoc a :errors errors)
      ;; recurrance:
      ;; (1) cyclic reference is detected when a fragment references
      ;; itself after transitive dependencies are added.
      ;; (2) the graph is updated to remove all nodes with no
      ;; references remaining (thus cannot participate in a cyclic
      ;; reference), or when the error for the cyclic reference has
      ;; been recorded.  The map is then updated to add dependencies
      (recur (->> (filter #(contains? (% 1) (% 0)) g)
                  (reduce #(err %1 (%2 0) "fragment '%s' contains a cyclic reference" (%2 0)) errors))
             (->> (remove (fn [[k v]] (or (contains? v k) (empty? v))) g)
                  (reduce (fn [g' [k v]]
                              (->> (map g v) ;; map a dependency to its frag reference sets
                                   (reduce set/union v) ;; union them together
                                   (filter g) ;; remove dependencies no longer in graph
                                   (into #{}) ;; put it back into a set, then assoc it to the updated graph
                                   (assoc g' k))) {}))))))

(declare check-selection-set)

(defn- check-field [tdef field-map a f]
  (case (:tag f)
    :selection-field
    (if-let [fdecl (field-map (:name f))]
      (let [[errors f] (check-selection-set (base-type (:type fdecl)) a (assoc f :resolved-type (:type fdecl)))]
        (-> (assoc a :errors errors)
            (update :vfields conj f)))
      (if (:alias f)
        (update a :errors err (:name f) "field '%s' (aliased as '%s') is not defined on type '%s'" (:name f) (:alias f) (:name tdef))
        (update a :errors err (:name f) "field '%s' is not defined on type '%s'" (:name f) (:name tdef))))

    :inline-fragment
    (let [on (get-in f [:on :name])]
      (if-let [ontype (get-in a [:schema :type-map on])]
        (if (contains? #{:type-definition :interface-definition :union-definition} (:tag ontype))
          a
          (update a :errors err on "inline fragment on non-composite type '%s'" on))
        (update a :errors err on "inline fragment on undefined type '%s'" on)))

    :fragment-spread
    (if-let [frag (get-in a [:fragment-map (:name f)])]
      a
      (update a :errors err (:name f) "fragment '%s' is not defined" (:name f)))))

(defn- check-selection-set [tname a def]
  (if-let [sset (:selection-set def)]
    (let [tdef (get-in a [:schema :type-map tname])
          field-map (:field-map tdef)
          a (reduce (partial check-field tdef field-map) (assoc a :vfields []) sset)]
      [(:errors a) (assoc def :selection-set (:vfields a))])
    [(:errors a) def]))

(defn- check-definition [a def]
  (case (:tag def)
    :selection-set
    (if (:anon a)
      (update a :errors err def "anonymous selection set is already declared")
      (let [[errors def] (check-selection-set (get-in a [:schema :roots :query]) a def)]
        (-> (assoc a :errors errors :anon def)
            (update :query conj def))))

    (:mutation :query-definition)
    (if (contains? (:def-map a) (:name def))
      ;; 5.1.1.1 operation name uniqueness
      (update a :errors err (:name def) "operation with name '%s' is already declared" (:name def))
      (let [[errors def] (check-selection-set (get-root (:schema a) def) a def)]
        (-> (assoc a :errors errors)
            (update :def-map assoc (:name def) def)
            (update :query conj def))))

    :fragment-definition
    (let [on (get-in def [:on :name])]
      (if-let [t (get-in a [:schema :type-map on])]
        (if (#{:type-definition :interface-definition :union-definition} (:tag t))
          (let [[errors def] (check-selection-set on a def)]
            (-> (assoc a :errors errors)
                (assoc-in [:fragment-map (:name def)] def)))
          ;; 5.4.1.3 Fragments on composite types
          (update a :errors err on "fragment on non-composite type '%s'" on))
        (update a :errors err on "fragment on undefined type '%s'" on)))))

(defn- check-definitions [a]
  (reduce check-definition (assoc a :query []) (:query a)))

;; 5.1.2.1 lone anonymous operation
;; must be called after check-definition
(defn- check-lone-anonymous [a]
  (if (and (:anon a) (not (empty? (:def-map a))))
    (update a :errors err (:anon a) "anonymous selection set must be lone operation")
    a))

(defn- check-fragment [a def]
  (if (contains? (:fragment-map a) (:name def))
    (update a :errors err (:name def) "fragment '%s' is already declared" (:name def))
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
       :def-map {}}
      (build-fragment-map)
      (check-fragment-cycles)
      (check-definitions)
      (check-lone-anonymous)
      (restructure)))
