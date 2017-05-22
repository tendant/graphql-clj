(ns graphql-clj.parser-test
  (:require [clojure.test :refer :all]
            [instaparse.core :as insta]
            [graphql-clj.parser :as parser]
            [yaml.core :as yaml]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]])
  (:import [java.io File]
           [graphql_clj Parser ParseException]))`>

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
  (let [em (meta expected) am (meta actual)]
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
                   (if (contains? expected k)
                     count
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

(defn- convert-loc [{s :start e :end}]
  {:start [(:line s) (:column s) (:index s)]
   :end [(:line e) (:column e) (:index e)]})
  ;; {:start [(:instaparse.gll/start-line m)
  ;;          (:instaparse.gll/start-column m)
  ;;          (:instaparse.gll/start-index m)]
  ;;  :end [(:instaparse.gll/end-line m)
  ;;        (:instaparse.gll/end-column m)
  ;;        (:instaparse.gll/end-index m)]})

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
    (when (or (> (assert-tree actual expect) 0)
              (not= actual expect))
      (printf "Writing parsed AST to '%s'%n" actual-file)
      (spit actual-file (pprint-edn actual) :encoding "UTF-8")
      (do-report {:type :fail
                  :actual actual
                  :expected expect}))))

(testing "primitive value tokenization"
  (are [input expect] (= (assoc expect :image input) (.parseValue (Parser. input)))
       "0" {:tag :int-value :value 0}
       "1" {:tag :int-value :value 1}
       "-2" {:tag :int-value :value -2}
       "34" {:tag :int-value :value 34}
       "1.2" {:tag :float-value :value 1.2}
       "1e3" {:tag :float-value :value 1e3}
       "1E3" {:tag :float-value :value 1e3}
       "1e+3" {:tag :float-value :value 1e3}
       "1e-3" {:tag :float-value :value 1e-3}
       "1.2e3" {:tag :float-value :value 1.2e3}
       "1.2E3" {:tag :float-value :value 1.2e3}
       "1.2e+3" {:tag :float-value :value 1.2e3}
       "1.2e-3" {:tag :float-value :value 1.2e-3}
       "\"\"" {:tag :string-value :value ""}
       "\"hello\"" {:tag :string-value :value "hello"}
       "\"\\\\,\\\",\\/,\\r,\\n,\\t,\\f,\\b\"" {:tag :string-value :value "\\,\",/,\r,\n,\t,\f,\b"}
       "\"\\u00a0\\u201C\"" {:tag :string-value :value "\u00a0\u201C"}
       "true" {:tag :boolean-value :value true}
       "false" {:tag :boolean-value :value false}
       "null" {:tag :null-value :value nil}
       "c" {:tag :enum-value :value 'c}))

(testing "tokenization errors"
  (are [input expect] (= expect (try (.parseValue (Parser. input))
                                     nil
                                     (catch ParseException ex
                                       {:loc (.location ex) :msg (.getMessage ex)})))
       ;; '-' must be followed by a digit, test non-digit and EOF
       "- "  {:loc {:line 1 :column 2 :index 1} :msg "expected digit after '-'"}
       "-"   {:loc {:line 1 :column 2 :index 1} :msg "expected digit after '-'"}
       ;; According to spec, 0-prefixed numbers are not allowed, and -0 is.
       "01"  {:loc {:line 1 :column 2 :index 1} :msg "zero-prefixed numbers are not allowed"}
       "-01" {:loc {:line 1 :column 3 :index 2} :msg "zero-prefixed numbers are not allowed"}
       ;; Decimal must be followed by at least 1 digit, test non-digit and EOF
       "2. " {:loc {:line 1 :column 3 :index 2} :msg "expected digit after '.'"}
       "2."  {:loc {:line 1 :column 3 :index 2} :msg "expected digit after '.'"}
       "0. " {:loc {:line 1 :column 3 :index 2} :msg "expected digit after '.'"}
       ;; Exponent must be followed by at least 1 digit, or '+' or '-' and a digit
       "3e " {:loc {:line 1 :column 3 :index 2} :msg "expected digit after 'e'"}
       "3e"  {:loc {:line 1 :column 3 :index 2} :msg "expected digit after 'e'"}
       "0e " {:loc {:line 1 :column 3 :index 2} :msg "expected digit after 'e'"}
       "3E " {:loc {:line 1 :column 3 :index 2} :msg "expected digit after 'e'"}
       "3e+ " {:loc {:line 1 :column 4 :index 3} :msg "expected digit after 'e'"}
       "3e+"  {:loc {:line 1 :column 4 :index 3} :msg "expected digit after 'e'"}
       "3E- " {:loc {:line 1 :column 4 :index 3} :msg "expected digit after 'e'"}
       "4.5e " {:loc {:line 1 :column 5 :index 4} :msg "expected digit after 'e'"}
       ;; Floats must start with an integer component, cannot just be the fractional component
       ".123" {:loc {:line 1 :column 1 :index 0} :msg "invalid character sequence, did you mean '...'?"}
       ;; Some characters are not allowed in the stream
       "\u0001" {:loc {:line 1 :column 1 :index 0} :msg "invalid character U+0001"}
       ;; EOF conditions in strings
       "\"x" {:loc {:line 1 :column 3 :index 2} :msg "unterminated string"}
       "\"\\" {:loc {:line 1 :column 3 :index 2} :msg "unterminated string"}
       "\"\\u123" {:loc {:line 1 :column 4 :index 3} :msg "unterminated string"}
       ;; Invalid characters in strings and escape sequences therein
       "\"x\n\"" {:loc {:line 1 :column 4 :index 3} :msg "invalid character in string"}
       "\"\\x1234\"" {:loc {:line 1 :column 4 :index 3} :msg "invalid escape sequence"}
       "\"\\uEFGH\"" {:loc {:line 1 :column 7 :index 6} :msg "invalid hex escape"}))

