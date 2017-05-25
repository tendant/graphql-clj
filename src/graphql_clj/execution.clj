(ns graphql-clj.execution
  (:require [graphql-clj.schema-validator :as sv]
            [graphql-clj.query-validator :as qv]
            [graphql-clj.resolver :as resolver]
            [graphql-clj.error :as gerror]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn- execute-operation
  [operation state]
  )

(defn- execute-document
  [document state operation-name]
  ;; FIXME: Should only execute one statement per request, need
  ;; additional paramter to specify which statement will be
  ;; executed. Current implementation will merge result from multiple
  ;; statements.
  (println "document:" document)
  (let [operations (if operation-name
                     (filter #(= (:operation-name %) operation-name) document)
                     document)
        operation (first operations)
        operation-count (count operations)]
    (cond
      (= 1 operation-count) (execute-operation operation state)
      (= 0 operation-count) {:errors [{:message (format "No such operation(%s)." operation-name)}]}
      (> 1 operation-count) {:errors [{:message (format "More than one operation has same name(%s)." operation-name)}]})))

;; Public API

(defn execute-validated-document
  ([context schema resolver-fn [statement-errors document] variables operation-name]
   (if (seq statement-errors)
     {:errors statement-errors}
     (execute-document document
                       {:variables (clojure.walk/stringify-keys variables)
                        :context context
                        :schema schema
                        :resolver (resolver/create-resolver-fn schema resolver-fn)}
                       operation-name)))
  ([context validated-schema resolver-fn validated-document]
   (execute-validated-document context validated-schema resolver-fn validated-document nil))
  ([context validated-schema resolver-fn validated-document variables]
   (execute-validated-document context validated-schema resolver-fn validated-document variables nil)))

(defn execute
  ([context string-or-validated-schema resolver-fn string-or-validated-document variables]
   (let [validated-schema (if (string? string-or-validated-schema)
                            (sv/validate-schema string-or-validated-schema)
                            string-or-validated-schema)
         validated-document (if (string? string-or-validated-document)
                              (try
                                (qv/validate-query validated-schema string-or-validated-document)
                                (catch Exception e
                                  [(:errors (ex-data e)) nil]))
                              string-or-validated-document)]
     (execute-validated-document context validated-schema resolver-fn validated-document variables)))
  ([context valid-schema resolver-fn string-or-validated-document]
   (execute context valid-schema resolver-fn string-or-validated-document nil)))


