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

(defn process-named-type [node]
  (case (first node)
    :namedType
    (symbol (second (second node)))))

(declare find-type)

(defn convert-list-type [node]
  (case (first node)
    :listType
    (reduce (fn process-list-type [v f]
              (cond
                (and (seq? f)
                     (= :type_ (first f))) (list 'list (find-type f))
                (#{:type_ "[" "]"} f) v) ; skip set
              )
            nil node)))

(defn find-type [node]
  (println "find-type: " node)
  (case (first node)
    :type_
    (reduce (fn process-type [v f]
              (println "v:" v)
              (println "f:" f)
              (cond
                (and (seq? f)
                     (= :namedType (first f))) (process-named-type f)
                (and (seq? f)
                     (= :listType (first f))) (let [t (convert-list-type f)]
                                                (println "listType result:" t)
                                                t)
                (and (= "!" f)) (list 'non-null v)
                (#{:type_} f) v ; skip set
                :else (do
                        (println "TODO: process-named-type:" f)
                        v)))
            nil node)))

(defn convert-field-arg [node]
  (println "convert-field-arg:" node)
  (case (first node)
    :inputValueDefinition
    (reduce (fn collect-arg [val arg]
              (cond
                (and (seq? arg)
                     (= :name (first arg))) (assoc val :name (second arg))
                (and (seq? arg)
                     (= :type_ (first arg))) (assoc val :type (find-type arg))
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
                     (= :type_ (first f))) (assoc m :type (find-type f))
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

(defn update-enum-type-definition [schema enum]
  (println "schema:" schema)
  (println "update-enum-type-definition:" enum)
  (assoc-in schema [:enums (keyword (:name enum))] (dissoc enum :name)))

(defn update-root-schema [schema root]
  (assoc-in schema [:roots] root))

(defn convert-union-type-members [node]
  (println "convert-union-type-members: node:" node)
  (if (= :unionMemberTypes (first node))
    (reduce (fn process-union-type-member [col f]
              (cond
                (and (seq? f)
                     (= :namedType (first f))) (conj col (process-named-type f))
                (#{:unionMemberTypes "=" "|"} f) col ; skip set
                ))
            [] node)))

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
                       (println "TODO: convert-fields" f)
                       m)))
            {} node)))

(defn convert-implements-interfaces [node]
  (case (first node)
    :implementsInterfaces
    (reduce (fn process-implements [col f]
              (println "col:" col)
              (println "f:" f)
              (cond
                (and (seq? f)
                     (= :namedType (first f))) (let [named-type (process-named-type f)]
                                                 (conj col (keyword named-type)))
                (#{:implementsInterfaces "implements"} f) col
                :else (do
                        (println "TODO: convert-implements-interfaces:" f)
                        col)))
            [] node)))

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
                (and (seq? f)
                     (= :implementsInterfaces (first f))) (assoc m :implements (convert-implements-interfaces f))
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

(defn convert-root-operation-type [node]
  (case (first node)
    :rootOperationTypeDefinition
    (reduce (fn process-root-operation [m f]
              (println "m:" m)
              (println "f:" f)
              (cond
                (and (seq? f)
                     (= :operationType (first f))) (assoc m :operation-type (second f))
                (and (seq? f)
                     (= :namedType (first f))) (assoc m :type (process-named-type f))
                (#{:rootOperationTypeDefinition ":"} f) m ; skip set
                :else (do
                        (println "TODO: convert-root-operation-type:" f)
                        m)))
            {} node)))

(defn convert-root-schema [node]
  (case (first node)
    :schemaDefinition
    (reduce (fn process-root-schema [m f]
              (println "m:" m)
              (println "f:" f)
              (cond
                (and (seq? f)
                     (= :rootOperationTypeDefinition (first f))) (let [root-operation (convert-root-operation-type f)]
                                                                     (assoc m (:operation-type root-operation) (:type root-operation)))
                (#{:schemaDefinition "schema" "{" "}"} f) m ; skip set
                :else (do
                        (println "TODO: process-root-schema:" f)
                        m)))
            {} node)))

(defn process-enum-value [node]
  (case (first node)
    :enumValueDefinition
    (let [enum-value (second node)]
      (cond
        (and (= :enumValue (first enum-value))
             (= :name (first (second enum-value)))) (keyword (second (second enum-value)))
        :else (do
                (println "TODO: process-enum-value:" enum-value)
                nil)))))

(defn convert-enum-values [node]
  (case (first node)
    :enumValuesDefinition
    (reduce (fn process-enum-values [col f]
              (println "col:" col)
              (println "f:" f)
              (cond
                (and (seq? f)
                     (= :enumValueDefinition (first f))) (conj col (process-enum-value f))
                (#{:enumValuesDefinition "{" "}"} f) col ; skip set
                :else (do
                        (println "TODO: process-enum-values:" f)
                        col)))
            [] node)))

(defn convert-enum-type-definition [node]
  (case (first node)
    :enumTypeDefinition
    (reduce (fn process-enum-type [m f]
              (println "m:" m)
              (println "f:" f)
              (cond
                (and (seq? f)
                     (= :name (first f))) (assoc m :name (second f))
                (and (seq? f)
                     (= :enumValuesDefinition (first f))) (assoc m :values (convert-enum-values f))
                (#{:enumTypeDefinition "enum" "{" "}"} f) m ; skip set
                :else (do
                        (println "TODO: process-enum-type:" f)
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
    :enumTypeDefinition (let [enum (convert-enum-type-definition node)]
                          (swap! *schema* update-enum-type-definition enum)
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
    :schemaDefinition (let [root-schema (convert-root-schema node)]
                        (swap! *schema* update-root-schema root-schema)
                        nil)
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