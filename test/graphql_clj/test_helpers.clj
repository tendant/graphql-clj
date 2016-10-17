(ns graphql-clj.test-helpers
  (:require [graphql-clj.parser :as parser]
            [clojure.walk :as w]
            [instaparse.core :as insta]
            [camel-snake-kebab.core :refer [->kebab-case]]
            [clojure.string :as str]))

(defn- parse-expectation [t]
  (let [then (:then t)]
    (if (map? then) (-> then keys first) (filter :error then))))

(defn- interpret-parsed [r]
  (cond (:reason r)                  :syntax-error
        (:operation-definitions r)   :passes
        (:type-system-definitions r) :passes
        (:error r)                   :validation-errors
        :else                        :parser-error))

(defn validate->ns [validate-str]
  (-> (str/join "." ["graphql-clj" "validator" "rules" (->kebab-case validate-str)])
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
             :parsed   parsed
             :expected (parse-expectation t')
             :result   (interpret-parsed parsed)})))

(defn parse-debug [stmt] (insta/parse #'parser/parser-fn stmt :partial true))

(defn visualize-debug [stmt] (-> stmt parser/parse insta/visualize))
