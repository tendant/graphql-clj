(ns graphql-clj.test-helpers
  (:require [graphql-clj.parser :as parser]
            [graphql-clj.validation :as validation]
            [clojure.walk :as w]
            [instaparse.core :as insta]
            [camel-snake-kebab.core :refer [->kebab-case]]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [yaml.core :as yaml]))

(defn- parse-expectation [t]
  (let [then (:then t)]
    (if (map? then) (-> then keys first) (filter :error then))))

(defn- interpret-parsed [r]
  (cond (:reason r)                  :syntax-error
        (:graphql-clj/operation-definitions r)   :passes
        (:graphql-clj/type-system-definitions r) :passes
        (:error r)                   :validation-errors
        ;; :else                        :parser-error
        :else r))

(defn- interpret-validated [v]
  (cond (nil? (get-in v [:state :errors])) :passes
        :else :error))

(defn validate->ns [validate-str]
  (-> (str/join "." ["graphql-clj" "validator" "rules" (->kebab-case validate-str)])
      (symbol "rules") resolve var-get))

(defn validation->ns [validate-str]
  (-> (str/join "." ["graphql-clj" "validation" "rules" (->kebab-case validate-str)])
      (symbol "rules") resolve var-get))

(defn parse-test-case [t]
  (let [t' (w/keywordize-keys t)
        query (get-in t' [:given :query])
        schema (get-in t' [:given :schema])
        parsed (parser/parse (or query schema))
        when' (:when t')]
    (cond-> {:name     (:name t')
             :type     (if schema :schema :query)
             :query    (or query schema)
             :schema   schema
             :when     when'
             :rules    (some->> (get when' :validate []) (mapcat validate->ns) vec)
             :validations    (some->> (get when' :validate []) (mapcat validation->ns) vec)
             :parsed   parsed
             :expected (parse-expectation t')
             :result   (interpret-parsed parsed)})))

(defn process-test-case [t]
  (let [t' (w/keywordize-keys t)
        query (get-in t' [:given :query])
        schema (get-in t' [:given :schema])
        parsed (parser/parse (or query schema))
        when' (:when t')
        when (first (keys when'))
        validations (some->> (get when' :validate []) (mapcat validation->ns) vec)
        validated (if (seq validations)
                    (validation/validate-document schema parsed validations))]
    {:name     (:name t')
     :type     (if schema :schema :query)
     :query    (or query schema)
     :schema   schema
     :validations    validations
     :parsed   parsed
     :validated validated
     :when     when
     :expected (parse-expectation t')
     :result (case when
               :parse (interpret-parsed parsed)
               :validate (interpret-validated validated))}))

(defn load-tests-from-yaml [r]
  (let [s (slurp (io/resource r))]
    (get (yaml/parse-string s) :tests)))

(defn load-schema [f]
  (let [s (slurp (io/resource f))]
    (parser/parse s)))

(defn parse-debug [stmt] (insta/parse #'parser/parser-fn stmt :partial true))

(defn visualize-debug [stmt] (-> stmt parser/parse insta/visualize))
