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
  (case (first node)
    :objectTypeDefinition
    (reduce (fn process-object-type [val f]
              (println "val:" val)
              (println "f:" f)
              (cond
                (and (seq? f)
                     (= :fieldsDefinition (first f))) (conj val f)
                (and (seq? f)
                     (= :name (first f))) (do (reset! *type* (second f))
                                              val)
                (#{:objectTypeDefinition "type"} f) val ; skip set
                :else (do
                        (println "TODO: type-definition:" f)
                        val)))
            [] node)))

(defn convert-fields-definition [node]
  (case (first node)
    :fieldsDefinition
    (reduce (fn collect-fields [val f]
              (println "val: " val)
              (println "f: " f)
              (cond
                (and (seq? f)
                     (= :fieldDefinition (first f))) (conj val f)
                (#{:fieldsDefinition "{" "}"} f) val ; skip set
                :else (do
                        (println "TODO: fields-definition:" f)
                        val)))
            [] node)))

(defn find-type [node]
  (println "find-type: " node)
  (case (first node)
    :namedType (second (second node))))

(defn convert-field-arg [node]
  (println "convert-field-arg:" node)
  (case (first node)
    :inputValueDefinition
    (reduce (fn collect-arg [val arg]
              (cond
                (and (seq? arg)
                     (= :name (first arg))) (assoc val :name (second arg))
                (and (seq? arg)
                     (= :type_ (first arg))) (assoc val :type (find-type (second arg)))
                :else (do
                        (println "TODO: field-arg:" arg)
                        val)))
            {} node)))

(defn convert-field-args [node]
  (println "convert-field-args:" node)
  (doseq [s node]
    (println "node s:" s))
  (case (first node)
    :argumentsDefinition
    (->> (filter seq? node)
         (map convert-field-arg)
         (map (fn [arg]
                [(:name arg) (dissoc arg :name)]))
         (into {}))))

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
                (and (seq? f)
                     (= :argumentsDefinition (first f))) (assoc m :args (convert-field-args f))
                (#{:fieldDefinition ":"} f) m ; skip set
                :else (do
                        (println "TODO: field-definition:" f)
                        m)))
            {} node)))

(defn update-type-field [schema type field]
  (println "schema:" schema)
  (println "update-type-field:" field)
  (assoc-in schema [:objects (keyword type) :fields (keyword (:name field))] (dissoc field :name)))

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
    :typeDefinition (do
                      (println "INFO: :typeDefinition:" (rest node))
                      (rest node))
    :document (do
                (println "INFO: :document:" (rest node))
                (rest node))
    :definition (do
                  (println "INFO: :definition:" (rest node))
                  (rest node))
    (do
      (println "TODO: convert-fn:" (first node))
      (rest node))
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