(ns graphql-clj.schema-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [graphql-clj.schema :as schema]))

(defmacro def-file-tests [dir-name bindings & body]
  (let [dir (io/as-file (io/resource dir-name)) ext (nth bindings 1)]
    (printf "dir: %s.%n" dir)
    (vec (for [file (.list dir) :when (string/ends-with? file ext) :let [name (string/replace file #"\..*$" "")]]
           `(deftest ~(symbol name)
              (let ~(->> (partition 2 bindings)
                         (reduce (fn [s [k v]] (conj s k (str dir-name "/" name v))) []))
                ~@body))))))

(defn test-file [in expected]
  (printf "testing: in file: %s, expected file: %s.%n" in expected)
  (let [label (last (string/split in #"/" ))
        path (.getParent (io/as-file in))
        input (slurp (io/resource in) :encoding "UTF-8")
        expect (if (and expected
                        (io/resource expected))
                 (edn/read-string (slurp (io/resource expected) :encoding "UTF-8")))
        actual (-> (case (string/replace label #"-.*$" "")
                     "schema" (schema/parse-schema input)
                     ;; "query" (parser/parse-query-document input)
                     ))
        actual-file (str "test/" path "/" label ".actual")]
    (when (or ; (> (assert-tree actual expect) 0)
              (not= actual expect))
      (println "***** Schema: ")
      (pprint actual)
      (printf "***** Schema Text:%n%s%n" (slurp (io/resource in)))
      (printf "***** Writing parsed AST to '%s'%n" actual-file)
      (pprint actual (io/writer actual-file))
      (do-report {:type :fail
                  :actual actual
                  :expected expect}))))

(def-file-tests "graphql_clj/schema_test"
  [in ".input" expected ".expected" actual-file ".actual"]
  (test-file in expected))

(defn test-schema-case
  [no]
  (test-file (format "graphql_clj/schema_test/%s.input" no)
             (format "graphql_clj/schema_test/%s.expected" no)))

(comment
  (test-file "graphql_clj/schema_test/schema-0001.input" "graphql_clj/schema_test/schema-0001.expected")

  ;; FIXME
  ;; (test-schema-case "schema-cats-016")
  (test-schema-case "schema-0008")
  )