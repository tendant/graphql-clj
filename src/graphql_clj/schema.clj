(ns graphql-clj.schema
  (:require [clj-antlr.core :as antlr]
            [clojure.walk :as walk]
            [clojure.java.io :as io]))

(def sample-1 "type Hello {
          world: String
        }")

;; https://github.com/graphql-java/graphql-java/tree/master/src/main/antlr
;; (def parser-java (-> "src/antlr/graphql-java/Graphql.g4"
;;                      antlr/parser))
(def parser-java (-> "antlr/graphql-java/Graphql.g4"
                     io/resource
                     slurp
                     antlr/parser))

;; https://github.com/walmartlabs/lacinia/blob/master/resources/com/walmartlabs/lacinia/schema.g4
;; (def parser-lacinia (-> "src/antlr/lacinia/schema.g4"
;;                         antlr/parser))

;; https://github.com/antlr/grammars-v4/blob/master/graphql/GraphQL.g4
(def parser (-> "graphql.g4"
                io/resource
                slurp
                antlr/parser))

(defn parse [schema-str]
  (antlr/parse parser-java schema-str))

(def ^:dynamic *state* (atom nil))

(def ^:dynamic *schema* (atom nil))

(def ^:dynamic *type* (atom nil))

(defn name? [node]
  (and (seq? node)
       (= :name (first node))))

(defn named-type? [node]
  (and (seq? node)
       (= :typeName (first node))))

(defn fields-definition? [node]
  (and (seq? node)
       (= :fieldsDefinition (first node))))

(defn node? [k node]
  (and (seq? node)
       (= k (first node))))

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

(defn convert-name [node]
  (println "convert-name:" node)
  (case (first node)
    :name (case (first (second node))
            :baseName (second (second node)))))

(defn convert-named-type [node]
  (case (first node)
    :typeName
    (let [name (convert-name (second node))]
      (case name
        ("String" "Float" "Int" "Boolean" "ID") (symbol name)
        (keyword name)))))

(declare find-type)
(declare convert-type)

(defn convert-list-type [node]
  (case (first node)
    :listType
    (reduce (fn process-list-type [v f]
              (cond
                (node? :type f) (list 'list (convert-type f))
                (#{:listType "[" "]"} f) v ; skip set

                :else (do
                        (println "TODO: process-list-type:" f)
                        v)))
            nil node)))

(defn convert-non-null-type [[tag :as node]]
  (case tag
    :nonNullType (list 'non-null (find-type (second node)))))

(defn find-type [[tag :as node]]
  (println "fidn-type:" node)
  (case tag
    :typeName (convert-named-type node)
    :nonNullType (convert-non-null-type node)
    :listType (convert-list-type node)))

(defn convert-type [node]
  (println "convert-type: " node)
  (case (first node)
    :type
    (find-type (second node))))

(defn convert-enum-base-name [node]
  (case (first node)
    :baseName (keyword (second node))))

(defn unroll [node col]
  (if (empty? col)
    (convert-enum-base-name node)
    (cond
      (node? (first col) node) (unroll (second node) (rest col))
      :else (do
              (println "TODO: unroll node:" node ". col:" col)
              (throw (ex-info (format "Failed unroll node: %s, col: %s." node col) {}))))))

(defn convert-value [[tag child :as node]]
  (case tag
    :value
    (case (first child)
      :booleanValue (boolean (second (second node)))
      :enumValue (unroll child [:enumValue :enumValueName]))))

(defn convert-default-value [node]
  (case (first node)
    :defaultValue
    (reduce (fn process-default-value [v f]
              (println "m:" v)
              (println "f:" v)
              (cond
                (and (seq? f)
                     (= :value (first f))) (if (nil? v)
                                             (convert-value f)
                                             (println "TODO: there are more than two default values."))
                (#{:defaultValue "="} f) v ; skip
                :else (do
                        (println "TODO: process-default-value:" f)
                        v)))
            nil node)))

(defn convert-input-value-definition [node]
  (case (first node)
    :inputValueDefinition
    (reduce (fn process-input-value-definition [m f]
              (println "m:" m)
              (println "f:" f)
              (cond
                (and (seq? f)
                     (= :name (first f))) (assoc m :name (convert-name f))
                (and (seq? f)
                     (= :type (first f))) (assoc m :type (convert-type f))
                (and (seq? f)
                     (= :defaultValue (first f))) (assoc m :default-value (convert-default-value f))
                (#{:inputValueDefinition ":"} f) m
                :else (do
                        (println "TODO: process-input-value-definition:" f)
                        m)))
            {} node)))

(defn convert-field-arg [node]
  (println "convert-field-arg:" node)
  (case (first node)
    :inputValueDefinition
    (reduce (fn collect-arg [m arg]
              (cond
                (and (seq? arg)
                     (= :name (first arg))) (assoc m :name (convert-name arg))
                (and (seq? arg)
                     (= :type (first arg))) (assoc m :type (convert-type arg))
                :else (do
                        (println "TODO: field-arg:" arg)
                        m)))
            {} node)))

(defn convert-field-args [node]
  (println "convert-field-args:" node)
  (doseq [s node]
    (println "node s:" s))
  (case (first node)
    :argumentsDefinition
    (reduce (fn process-argument-definition [m f]
              (println "m:" m)
              (println "f:" f)
              (cond
                (and (seq? f)
                     (= :inputValueDefinition (first f))) (let [input (convert-input-value-definition f)]
                                                            (assoc m (:name input) (dissoc input :name)))
                (#{:argumentsDefinition "(" ":" ")"} f) m ; skip
                :else (do
                        (println "TODO: process-argument-definition" f)
                        m)))
            {} node)))

(defn convert-field-definition [node]
  (println "convert-field-definition: " node)
  (case (first node)
    :fieldDefinition
    (reduce (fn process-field-definition [m f]
              (println "process-field-definition:")
              (println "m: " m)
              (println "f: " f)
              (cond
                (and (seq? f)
                     (= :name (first f))) (assoc m :name (convert-name f))
                (node? :type f) (assoc m :type (convert-type f))
                (and (seq? f)
                     (= :argumentsDefinition (first f))) (assoc m :args (convert-field-args f))
                (#{:fieldDefinition ":" "{" "}"} f) m ; skip set
                :else (do
                        (println "TODO: process-field-definition:" f)
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
  (assoc-in schema [:interfaces (keyword (:name interface))] (dissoc interface :name)))

(defn update-enum-type-definition [schema enum]
  (println "schema:" schema)
  (println "update-enum-type-definition:" enum)
  (assoc-in schema [:enums (keyword (:name enum))] (dissoc enum :name)))

(defn update-scalar-type-definition [schema scalar]
  (println "schema:" schema)
  (println "update-scalar-type-definition:" scalar)
  (assoc-in schema [:scalars (keyword (:name scalar))] (dissoc scalar :name)))

(defn update-input-type-definition [schema input]
  (println "schema:" schema)
  (println "update-input-type-definition:" input)
  (assoc-in schema [:input-objects (keyword (:name input))] (dissoc input :name)))

(defn update-root-schema [schema root]
  (assoc-in schema [:roots] root))

(defn convert-union-type-members [node]
  (case (first node)
    :unionMembers
    (reduce (fn process-union-type-member [col f]
              (cond
                (named-type? f) (conj col (convert-named-type f))
                (node? :unionMembers f) (concat col (convert-union-type-members f))
                (#{:unionMembers "=" "|"} f) col ; skip set
                :else (do
                        (println "TODO: process-union-type-member:" f)
                        col)))
            [] node)))

(defn convert-union-membership [node]
  (case (first node)
    :unionMembership
    (reduce (fn process-union-type-members [col f]
              (cond
                (node? :unionMembers f) (concat col (convert-union-type-members f))
                (#{:unionMemebership "="} f) col ; skip
                :else (do
                        (println "TODO: process-union-type-members" f)
                        col)))
            [] node)))

(defn convert-union-type-definition [node]
  (if (= :unionTypeDefinition (first node))
    (reduce (fn process-union-type-definition [m f]
              (println "m:" m)
              (println "f:" f)
              (cond
                (name? f) (assoc m :name (convert-name f))
                (and (seq? f)
                     (= :unionMembership (first f))) (assoc m :members (into [] (convert-union-membership f)))
                (#{:unionTypeDefinition "union"} f) m ; skip
                :else (do
                        (println "TODO: process-union-type-definition" f)
                        m)))
            {} node)))

(defn convert-fields [node]
  (case (first node)
    :fieldsDefinition
    (reduce (fn process-field-definition [m f]
              (println "m:" m)
              (println "f:" f)
              (cond
                (name? f) (assoc m :name (convert-name f))
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
                (named-type? f) (let [named-type (convert-named-type f)]
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
                (name? f) (assoc m :name (convert-name f))
                (fields-definition? f) (assoc m :fields (convert-fields f))
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
                     (= :name (first f))) (assoc m :name (convert-name f))
                (fields-definition? f) (assoc m :fields (convert-fields f))
                (#{:interfaceTypeDefinition "interface" "{" "}"} f) m ; skip set
                :else (do
                        (println "TODO: convert-interface-type-definition:" f)
                        m)))
            {} node)))

(defn convert-root-operation-type [node]
  (case (first node)
    :operationTypeDefinition
    (reduce (fn process-root-operation [m f]
              (println "m:" m)
              (println "f:" f)
              (cond
                (node? :operationType f) (assoc m :operation-type (second f))
                (node? :typeName f) (assoc m :type (convert-named-type f))
                (#{:operationTypeDefinition ":"} f) m ; skip set
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
                (node? :operationTypeDefinition f) (let [root-operation (convert-root-operation-type f)]
                                                     (assoc m (keyword (:operation-type root-operation)) (:type root-operation)))
                (#{:schemaDefinition "schema" "{" "}"} f) m ; skip set
                :else (do
                        (println "TODO: process-root-schema:" f)
                        m)))
            {} node)))

(defn process-enum-value [node]
  (case (first node)
    :enumValueDefinition
    (unroll node [:enumValueDefinition :enumValue :enumValueName])))

(defn convert-enum-values [node]
  (case (first node)
    :enumValueDefinitions
    (reduce (fn process-enum-values [col f]
              (println "col:" col)
              (println "f:" f)
              (cond
                (and (seq? f)
                     (= :enumValueDefinition (first f))) (conj col (process-enum-value f))
                (#{:enumValueDefinitions "{" "}"} f) col ; skip set
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
                     (= :name (first f))) (assoc m :name (convert-name f))
                (and (seq? f)
                     (= :enumValueDefinitions (first f))) (assoc m :values (convert-enum-values f))
                (#{:enumTypeDefinition "enum" "{" "}"} f) m ; skip set
                :else (do
                        (println "TODO: process-enum-type:" f)
                        m)))
            {} node)))

(defn convert-scalar-type-definition [node]
  (case (first node)
    :scalarTypeDefinition
    (reduce (fn process-scalar-type [m f]
              (println "m:" m)
              (println "f:" f)
              (cond
                (and (seq? f)
                     (= :name (first f))) (assoc m :name (convert-name f))
                :else (do
                        (println "TODO: process-scalar-type:" f)
                        m)))
            {} node)))

(defn convert-input-fields-definition [node]
  (case (first node)
    :inputFieldsDefinition
    (reduce (fn process-field-definition [m f]
              (println "m:" m)
              (println "f:" f)
              (cond
                (and (seq? f)
                     (= :inputValueDefinition (first f))) (let [input-value (convert-input-value-definition f)]
                                                            (assoc m (keyword (:name input-value)) (dissoc input-value :name)))
                :else (do
                        (println "TODO: process-field-definition:" f)
                        m)))
            {} node)))

(defn convert-input-type-definition [node]
  (case (first node)
    :inputObjectTypeDefinition
    (reduce (fn process-input-type [m f]
              (println "m:" m)
              (println "f:" f)
              (cond
                (and (seq? f)
                     (= :name (first f))) (assoc m :name (convert-name f))
                (and (seq? f)
                     (= :inputFieldsDefinition (first f))) (assoc m :fields (convert-input-fields-definition f))
                :else (do
                        (println "TODO: process-input-type:" f)
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
    :scalarTypeDefinition (let [scalar (convert-scalar-type-definition node)]
                            (swap! *schema* update-scalar-type-definition scalar)
                            nil)
    :inputObjectTypeDefinition (let [input (convert-input-type-definition node)]
                                 (swap! *schema* update-input-type-definition input)
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