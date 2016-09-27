(ns cats.scenarios.test-helpers
  (:require [graphql-clj.parser :as parser]))

(defn- parse-expectation [t]
  (let [then (:then t)]
    (if (map? then) (-> then keys first) (filter :error then))))

(defn- interpret-parsed [r]
  (cond (:reason r)                :syntax-error
        (:operation-definitions r) :passes
        (:error r)                 :validation-errors
        :else                      :parser-error))

(defn parse-test-case [t]
  (let [t' (clojure.walk/keywordize-keys t)
        q (get-in t' [:given :query])
        parsed (parser/parse q)]
    (cond-> {:name     (:name t')
             :query    q
             :when     (:when t')
             :parsed   parsed
             :expected (parse-expectation t')
             :result   (interpret-parsed parsed)})))
