(ns graphql-clj.parser-test
  (:require [clojure.test :refer :all]
            [instaparse.core :as insta]
            [graphql-clj.parser :as parser]
            [yaml.core :as yaml]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]])
  (:import [java.io File]))

(defn- local-resource [^String src]
  (-> local-resource .getClass (.getResource src) .getPath File.))

(defmacro def-file-tests [dir-name bindings & body]
  (let [dir (local-resource dir-name) ext (nth bindings 1)]
    (vec (for [file (.list dir) :when (str/ends-with? file ext) :let [name (str/replace file #"\..*$" "")]]
           `(deftest ~(symbol name)
              (let ~(->> (partition 2 bindings)
                         (reduce (fn [s [k v]] (conj s k `(File. ~(.toString (File. dir (str name v)))))) []))
                ~@body))))))

(defn- fail [count path actual expected fmt & args]
  (do-report {:type :fail
              :actual actual
              :expected expected
              :message (str "At " (if (empty? path) "{root}" (str/join "/" path)) ": " (apply format fmt args))})
  (inc count))


(declare assert-tree)
(defn- assert-meta [count path actual expected]
  (let [em (meta expected) am (meta expected)]
    (cond (and em am) (assert-tree count (conj path "^") am em)
          em (fail count path am em "Expected meta data")
          am (fail count path am em "Unexpected meta data")
          :else count)))

(defn- assert-tree
  ([actual expected] (assert-tree 0 [] actual expected))
  ([count path actual expected]
   (let [count (assert-meta count path actual expected)]
     (cond
       (map? expected)
       (if-not (map? actual)
         (fail count path "not a map" "a map" "type-mismatch")
         (letfn [(check-actual [count k]
                   (if (contains? actual k)
                     (assert-tree count (conj path k) (actual k) (expected k))
                     (fail count path (actual k) (expected k) "Missing key '%s'" k)))
                 (check-expected [count k]
                   (if (contains? expected k) count
                     (fail count path (actual k) (expected k) "Extra key '%s'" k)))]
           (let [count (reduce check-expected count (keys expected))]
             (reduce check-actual count (keys actual)))))
    
       (sequential? expected)
       (if-not (sequential? actual)
         (fail count path "not a sequence" "a sequence" "type-mismatch")
         (loop [count count a actual e expected i 0]
           (cond
             (empty? a)
             (if-not (empty? e)
               (fail count path nil (first e) "Missing element at index %d" i)
               count)
             
             (empty? e)
             (fail count path (first a) nil "Extra elements at index %d" i)
             
             :else
             (recur (assert-tree count (conj path i) (first a) (first e))
                    (rest a) (rest e) (inc i)))))

       :else
       (if-not (= actual expected)
         (fail count path actual expected "data mismatch")
         count)))))

(def ^:private first-keys [:tag :name :type])
(def ^:private first-key-set (into #{} first-keys))

(defn- kv-order
  "pprint-edn helper function that returns a map's key-value pairs in
  the following order: all non-collection values appear first,
  followed by all collection values.  Within each group, the keys are
  sorted such that keys in 'first-keys' appear first, and the rest
  appear alphabetically."
  [obj]
  (let [k (apply sorted-set (keys obj))
        k (->> (concat (filter k first-keys) (remove first-key-set k))
               (group-by #(coll? (obj %))))]
    (->> (concat (k false) (k true))
         (map #(vector % (obj %))))))

(defn- simple-map? [obj] (and (map? obj) (not-any? coll? (vals obj))))

(defn- convert-loc [m]
  {:start [(:instaparse.gll/start-line m)
           (:instaparse.gll/start-column m)
           (:instaparse.gll/start-index m)]
   :end [(:instaparse.gll/end-line m)
         (:instaparse.gll/end-column m)
         (:instaparse.gll/end-index m)]})

(defn- simplify-location-meta [obj]
  (let [r (cond (map? obj) (into {} (map simplify-location-meta) obj)
                (coll? obj) (into [] (map simplify-location-meta) obj)
                :else obj)]
    (if-let [m (meta obj)] (with-meta r (convert-loc m)) r)))

(defn- remove-ns-from [obj]
  (let [r (cond (map? obj) (into {} (map remove-ns-from obj))
                (coll? obj) (into [] (map remove-ns-from obj))
                (keyword? obj) (keyword (name obj))
                (symbol? obj) (symbol (name obj))
                :else obj)]
    (if-let [m (meta obj)] (with-meta r (remove-ns-from m)) r)))
        
(defn- pprint-edn
  ([obj] (pprint-edn "" obj))
  ([indent obj]
   (let [subi (str indent " ")]
     (str (if-let [m (meta obj)] (str "^" (pr-str m) (if (coll? obj) (str "\n" indent) " ") ""))
          (cond
            (map? obj)
            (str "{"
                 (->> (for [[k v] (kv-order obj)]
                        (str (pr-str k) (if (coll? v) (str "\n" subi) " ") (pprint-edn subi v)))
                      (str/join (str "\n" subi)))
                 "}")
            
            (sequential? obj)
            (str "[" (str/join (str "\n" subi) (map #(pprint-edn subi %) obj)) "]")
        
            :else
            (pr-str obj))))))
    

(def-file-tests "parser_test"
  [in ".input" expected ".expected.edn" actual-file ".actual.edn"]
  (let [input (slurp in :encoding "UTF-8")
        expect (if (.exists expected) (edn/read-string (slurp expected :encoding "UTF-8")))
        actual (-> (case (str/replace (.getName in) #"-.*$" "")
                     "schema" (parser/parse-schema input)
                     "query" (parser/parse-query-document input))
                   (simplify-location-meta))]
    (if (> (assert-tree actual expect) 0)
      (do 
        (printf "Writing parsed AST to '%s'%n" actual-file)
        (spit actual-file (pprint-edn actual) :encoding "UTF-8")))
      (is (= actual expect))))




;; (comment
;; (def test-statements (edn/read-string (slurp "test/scenarios/statements.edn")))

;; (deftest parse-statements
;;   (doseq [statement test-statements]
;;     (testing (str "Test all statements parsing, statement: " statement)
;;       (is (not (insta/failure? (parser/parse-query-document statement)))))))

;; (def test-schemas (conj (edn/read-string (slurp "test/scenarios/schemas.edn"))
;;                         (slurp "test/scenarios/cats/validation/validation.schema.graphql")))

;; (deftest parse-schemas
;;   (doseq [schema test-schemas]
;;     (testing (str "Test schema parsing and transforming. schema: " schema)
;;       (let [result (parser/parse-schema schema)]
;;         (if (insta/failure? result) (println result))
;;         (is (not (insta/failure? result)))))))

;; (defn schema-parser-tests [type filename]
;;   (->> (get (yaml/from-file filename) "tests")
;;        (map (partial th/parse-test-case type))))

;; (println "HERE!")
;; (doseq [e (schema-parser-tests :schema "test/scenarios/cats/parsing/SchemaParser.yaml")]
;;   (println e))

;; ;; (deftest parse-cats-yaml
;; ;;   (testing "SchemaParser.yaml"
;; ;;     (doseq [{:keys [expected result]} (schema-parser-tests :schema "test/scenarios/cats/parsing/SchemaParser.yaml")]
;; ;;       (is (= expected result))))
;; ;;   (testing "DefaultValuesOfCorrectType.yaml"
;; ;;     (doseq [{:keys [result]} (schema-parser-tests :query "test/scenarios/cats/validation/DefaultValuesOfCorrectType.yaml")]
;; ;;       (is (= :passes result)))))

;; (def type-fields-kv-example
;;   "type Hello {
;;      world(flag: Boolean = true): String!
;;      this: Int
;;    }")

;; (def input-type-fields-kv-example
;;   "input Hello {
;;     world: String!
;;     this: Int
;;    }")

;; (def variable-kv-example
;;   "query WithDefaultValues(
;;      $a: Int = 1,
;;      $b: String! = \"ok\",
;;      $c: ComplexInput = { requiredField: true, intField: 3 }) {
;;        dog { name }
;;      }")

;; (def enum-argument-example
;;   "{
;;     empireHero: hero(episode: EMPIRE) {
;;       name
;;     }
;;     jediHero: hero(episode: JEDI) {
;;       name
;;     }
;;   }")

;; (deftest parse-kv-pairs
;;   (testing "we can convert type-fields to a map"
;;     (is (= (-> (parser/parse-schema type-fields-kv-example) :type-system-definitions first :fields)
;;            [{:node-type :type-field
;;              :field-name "world"
;;              :type-name "String"
;;              :required true
;;              :arguments [{:node-type :type-field-argument
;;                           :argument-name "flag"
;;                           :type-name "Boolean"
;;                           :default-value true}]}
;;             {:node-type :type-field
;;              :field-name "this"
;;              :type-name "Int"}])))
;;   (testing "we can convert input-type-fields to a map"
;;     (is (= (-> (parser/parse-schema input-type-fields-kv-example) :type-system-definitions first :fields)
;;            [{:node-type :input-type-field
;;              :field-name "world"
;;              :type-name "String"
;;              :required true}
;;             {:node-type :input-type-field :field-name "this" :type-name "Int"}])))
;;   (testing "we can convert variables to a map"
;;     (is (= (-> (parser/parse-query-document variable-kv-example) :operation-definitions first :variable-definitions)
;;            [{:node-type :variable-definition :variable-name "a" :type-name "Int" :default-value 1}
;;             {:node-type :variable-definition :variable-name "b" :type-name "String" :required true :default-value "ok"}
;;             {:node-type :variable-definition :variable-name "c" :type-name "ComplexInput" :default-value [:object-value [{:name "requiredField" :value true}
;;                                                                                                                          {:name "intField" :value 3}]]}])))
;;   (testing "we can convert enum arguments"
;;     (is (= (-> (parser/parse-schema enum-argument-example) :operation-definitions first :selection-set)
;;            [{:node-type     :field
;;              :name          "empireHero"
;;              :field-name    "hero"
;;              :arguments     [{:node-type     :argument
;;                               :argument-name "episode"
;;                               :value         "EMPIRE"}]
;;              :selection-set [{:node-type :field :field-name "name"}]}
;;             {:node-type     :field
;;              :name          "jediHero"
;;              :field-name    "hero"
;;              :arguments     [{:node-type     :argument
;;                               :argument-name "episode"
;;                               :value         "JEDI"}]
;;              :selection-set [{:node-type :field :field-name "name"}]}]))))

;; ;;;;; Test Helpers for visualizing the parsed tree ;;;;;

;; (defn render-parsed! [to-filename parsed-data]
;;   (pp/pprint parsed-data (clojure.java.io/writer (str "test/scenarios/parsed/" to-filename))))

;; (defn render-all-parsed! []
;;   (render-parsed! "statements.edn" (mapv parser/parse-query-document test-statements))
;;   (render-parsed! "schemas.edn" (mapv parser/parse-schema test-schemas)))

;; ;; To generate a large parsed tree for inspection from the repl
;; (comment
;;   (require '[graphql-clj.parser-test :as pt])
;;   (pt/render-all-parsed!))
;; ;; Go look in test/scenarios/parsed/schemas.edn and statements.edn
;; )
