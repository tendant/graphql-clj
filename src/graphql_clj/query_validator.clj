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

(defn- check-selection-set
  [schema errors fragmap tname sset]
  (let [tdef (get-in schema [:type-map tname])
        fieldmap (:field-map tdef)]
    (if (nil? (:selection-set sset))
      [errors sset]
      (loop [errors errors vfields [] [f & fields] (:selection-set sset)]
        (if f
          (case (:tag f)
            :selection-field
            (if-let [fdecl (fieldmap (:name f))]
              (let [[errors f] (check-selection-set schema errors fragmap (base-type (:type fdecl)) f)]
                (recur errors (conj vfields (assoc f :resolved-type (:type fdecl))) fields))
              (recur (if (:alias f)
                       (err errors (:name f) "field '%s' (aliased as '%s') is not defined on type '%s'" (:name f) (:alias f) tname)
                       (err errors (:name f) "field '%s' is not defined on type '%s'" (:name f) tname))
                     vfields
                     fields))

            :inline-fragment
            (let [on (get-in f [:on :name])]
              (if-let [ontype (get-in schema [:type-map on])]
                (case (:tag ontype)
                  (:type-definition :interface-definition :union-definition)
                  (recur errors vfields fields) ;; TODO: check-selection-set recurse
                  (recur (err errors on "inline fragment on non-composite type '%s'" on) vfields fields))
                (recur (err errors on "inline fragment on undefined type '%s'" on) vfields fields)))
             
            :fragment-spread
            (if-let [frag (fragmap (:name f))]
              (recur errors vfields fields)
              (recur (err errors (:name f) "fragment '%s' is not defined" (:name f)) vfields fields)))
          [errors (assoc sset :selection-set vfields)])))))

(defn- build-fragment-map [errors schema query]
  (loop [errors errors fmap {} [def & defs] (filter #(= :fragment-definition (:tag %)) query)]
    (cond
      (nil? def) [errors fmap]
      (contains? fmap (:name def)) (recur (err errors (:name def) "fragment '%s' is already declared" (:name def)) fmap defs)
      :else (recur errors (assoc fmap (:name def) def) defs))))

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

(defn- check-operations [errors schema fmap query]
  (loop [errors errors vdefs [] anon nil dmap {} fmap' fmap [def & defs] query]
    (if def
      (case (:tag def)
        :selection-set
        (if anon
          ;; 5.1.2.1 lone anonymous operation
          (recur (err errors def "anonymous selection set is already declared") vdefs anon dmap fmap' defs)
          (let [[errors def] (check-selection-set schema errors fmap (get-in schema [:roots :query]) def)] 
            (recur errors (conj vdefs def) def dmap fmap' defs)))

        (:mutation :query-definition)
        (if (contains? dmap (:name def))
          ;; 5.1.1.1 operation name uniqueness
          (recur (err errors (:name def) "operation with name '%s' is already declared" (:name def)) vdefs anon dmap fmap' defs)
          (let [[errors def] (check-selection-set schema errors fmap (get-root schema def) def)]
            (recur errors (conj vdefs def) anon (assoc dmap (:name def) def) fmap' defs)))

        :fragment-definition
        (let [on (get-in def [:on :name])]
          (if-let [t (get-in schema [:type-map on])]
            (if (#{:type-definition :interface-definition :union-definition} (:tag t))
              (let [[errors def] (check-selection-set schema errors fmap on def)]
                (recur errors vdefs anon dmap (assoc fmap' (:name def) def) defs))
              ;; 5.4.1.3 Fragments on composite types
              (recur (err errors on "fragment on non-composite type '%s'" on) vdefs anon dmap fmap' defs))
            (recur (err errors on "fragment on undefined type '%s'" on) vdefs anon dmap fmap' defs))))

      ;; else, end of defs:
      ;; 5.1.2.1 lone anonymous operation
      [(if (and anon (not (empty? dmap)))
         (err errors anon "anonymous selection set must be lone operation")
         errors)
       vdefs])))
  
(defn validate-query
  [schema query]
  (let [[errors fmap] (build-fragment-map [] schema query)
        fragment-errors (check-fragment-cycles errors fmap)
        [errors vquery] (check-operations fragment-errors schema fmap query)]
    [(vec errors) vquery]))
