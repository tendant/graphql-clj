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
              (println "process-field-definition:")
              (println "m: " m)
              (println "f: " f)
              (cond
                (and (seq? f)
                     (= :name (first f))) (assoc m :name (second f))
                (and (seq? f)
                     (= :type_ (first f))) (assoc m :type (find-type (second f)))
                (and (seq? f)
                     (= :argumentsDefinition (first f))) (assoc m :args (convert-field-args f))
                (#{:fieldDefinition ":" "{" "}"} f) m ; skip set
                :else (do
                        (println "TODO: field-definition:" f)
                        m)))
            {} node)))

(defn update-type-field [schema type field]
  (println "schema:" schema)
  (println "update-type-field:" field)
  (assoc-in schema [:objects (keyword type) :fields (keyword (:name field))] (dissoc field :name)))

(defn update-type-definition [schema type-definition]
  (println "schema:" schema)
  (println "update-type-definition:" type-definition)
  (assoc-in schema [:objects (keyword (:name type-definition))] (dissoc type-definition :name)))

(defn update-union-type-definition [schema union]
  (println "schema:" schema)
  (println "update-union-type-definition:" union)
  (assoc-in schema [:unions (keyword (:name union))] union))

(defn update-interface-type-definition [schema interface]
  (println "schema:" schema)
  (println "update-interface-type-definition:" interface)
  (assoc-in schema [:interfaces (keyword (:name interface))] interface))

(defn convert-union-type-members [node]
  (println "node:" node)
  (if (= :unionMemberTypes (first node))
    (->> node
         (filter seq?)
         (map find-type)
         (into []))))

(defn convert-union-type-definition [node]
  (if (= :unionTypeDefinition (first node))
    (reduce (fn process-union-type-definition [m f]
              (println "m:" m)
              (println "f:" f)
              (cond
                (and (seq? f)
                     (= :name (first f))) (assoc m :name (second f))
                (and (seq? f)
                     (= :unionMemberTypes (first f))) (assoc m :members (convert-union-type-members f))
                :else (do
                        (println "TODO: union-type-field" f)
                        m)))
            {} node)))

(defn convert-fields [node]
  (case (first node)
    :fieldsDefinition
    (reduce (fn process-field-definition [m f]
              (println "m:" m)
              (println "f:" f)
              (cond
                (and (seq? f)
                     (= :name (first f))) (assoc m :name (second f))
                (and (seq? f)
                     (= :fieldDefinition (first f))) (let [field (convert-field-definition f)]
                                                       (assoc m (keyword (:name field)) (dissoc field :name)))
                (#{:fieldsDefinition ":" "{" "}"} f) m ; skip set
                :else (do
                       (println "TODO: convert-fields" f (= :fieldDefinition (first f)) (seq? f))
                       m)))
            {} node)))

(defn convert-type-definition [node]
  (case (first node)
    :objectTypeDefinition
    (reduce (fn process-object-type [m f]
              (println "m:" m)
              (println "f:" f)
              (cond
                (and (seq? f)
                     (= :name (first f))) (assoc m :name (second f))
                (and (seq? f)
                     (= :fieldsDefinition (first f))) (assoc m :fields (convert-fields f))
                (#{:objectTypeDefinition "type"} f) m ; skip set
                :else (do
                        (println "TODO: convert-type-definition:" f)
                        m)))
            {} node)))


(defn convert-interface-type-definition [node]
  (println "convert-interface-type-definition:" node)
  (case (first node)
    :interfaceTypeDefinition
    (reduce (fn process-interface-type [m f]
              (println "process-interface-type:" m f)
              (println "m:" m)
              (println "f:" f)
              (cond
                (and (seq? f)
                     (= :name (first f))) (assoc m :name (second f))
                (and (seq? f)
                     (= :fieldsDefinition (first f))) (assoc m :fields (convert-fields f))
                (#{:interfaceTypeDefinition "interface" "{" "}"} f) m ; skip set
                :else (do
                        (println "TODO: convert-interface-type-definition:" f)
                        m)))
            {} node)))

(defn convert-fn [node]
  (println "node: " node)
  ;; (println "rest:" (rest node))
  (case (first node)
    :typeSystemDefinition (do
                            (println "typeSystemDefinition: schema:" @*schema*)
                            (println "processing node:" node)
                            (rest node))
    :objectTypeDefinition (let [type-definition (convert-type-definition node)]
                            (println "objectTypeDefinition:" type-definition)
                            (swap! *schema* update-type-definition type-definition)
                            nil)
    ;; :fieldsDefinition (convert-fields-definition node)
    ;; :fieldDefinition (let [field (convert-field-definition node)
    ;;                        name (:name field)]
    ;;                    (println "fieldDefinition: field" field)
    ;;                    (swap! *schema* update-type-field @*type* field)
    ;;                    nil)
    :unionTypeDefinition (let [union (convert-union-type-definition node)]
                           (swap! *schema* update-union-type-definition union)
                           nil)
    :interfaceTypeDefinition (let [interface (convert-interface-type-definition node)]
                               (swap! *schema* update-interface-type-definition interface)
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
      (println "TODO: convert-fn:" node)
      (rest node))
    ))

(defn convert [s]
  (try
    (reset! *schema* {})
    (reset! *type* nil)
    (reset! *state* nil)
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