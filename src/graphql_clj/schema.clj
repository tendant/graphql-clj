(ns graphql-clj.schema
  (:require [clj-antlr.core :as antlr]
            [clojure.walk :as walk]
            [clojure.java.io :as io]))

(def sample-1 "type Hello {
          world: String
        }")

(def parser (-> "graphql.g4"
                io/resource
                slurp
                antlr/parser))

(defn parse [schema-str]
  (antlr/parse parser schema-str))

(def ^:dynamic *state* (atom nil))

(def ^:dynamic *schema* (atom nil))

(def ^:dynamic *type* (atom nil))

(defn convert-type-definition [node]
  (if (= :objectTypeDefinition (first node))
    (reduce (fn process-object-type [val f]
              (println "val:" val)
              (println "f:" f)
              (cond
                (and (seq? f)
                     (= :fieldsDefinition (first f))) (conj val f)
                (and (seq? f)
                     (= :name (first f))) (do (reset! *type* (second f))
                                              val)
                :else val))
            [] node)))

(defn convert-fields-definition [node]
  (if (= :fieldsDefinition (first node))
    (reduce (fn collect-fields [val f]
              (println "val: " val)
              (println "f: " f)
              (cond
                (and (seq? f)
                     (= :fieldDefinition (first f))) (conj val f)
                :else val))
            [] node)))

(defn find-type [node]
  (println "find-type: " node)
  (case (first node)
    :namedType (second (second node))))

(defn convert-field-definition [node]
  (println "convert-field-definition: " node)
  (if (= :fieldDefinition (first node))
    (reduce (fn process-field-definition [m f]
              (println "m: " m)
              (println "f: " f)
              (cond
                (and (seq? f)
                     (= :name (first f))) (assoc m :name (second f))
                (and (seq? f)
                     (= :type_ (first f))) (assoc m :type (find-type (second f)))
                :else m))
            {} node)))

(defn update-type-field [schema type field]
  (println "schema:" schema)
  (println "update-type-field:" field)
  (assoc-in schema [:objects (keyword type) :fields (keyword (:name field))] field))

(defn convert-fn [node]
  (println "node: " node)
  ;; (println "rest:" (rest node))
  (case (first node)
    :typeSystemDefinition (do (reset! *schema* {})
                              (rest node))
    :objectTypeDefinition (convert-type-definition node)
    :fieldsDefinition (convert-fields-definition node)
    :fieldDefinition (let [field (convert-field-definition node)
                           name (:name field)]
                       (println "fieldDefinition: field" field)
                       (swap! *schema* update-type-field @*type* field)
                       nil)
    (rest node)
    ))

(defn convert [s]
  (try
    (walk/prewalk convert-fn s)
    @*schema*
    (catch Exception e
      (println e))
    (finally
      (println "*schema*:" @*schema*)
      (println "*type*:" @*type*)
      (println "*state*:" @*state*))))

(defn parse-schema [s]
  (convert (parse s)))