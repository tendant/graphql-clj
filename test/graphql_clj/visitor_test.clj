(ns graphql-clj.visitor-test
  (:require [clojure.test :refer :all]
            [graphql-clj.visitor :as visitor]
            [graphql-clj.spec :as spec]
            [clojure.data]))


(def document
  {:type-system-definitions
   [{:node-type :schema-definition
     :query-type {:name "QueryRoot"}
     :section :type-system-definitions
     :kind :SCHEMA}
    {:node-type :type-definition
     :type-name "QueryRoot"
     :section :type-system-definitions
     :fields  [{:node-type :type-field :field-name "person" :type-name "Person"
                :arguments [{:node-type :type-field-argument :argument-name "id" :type-name "Int"}]}]
     :kind :OBJECT}
    {:node-type :type-definition
     :type-name "Person"
     :section   :type-system-definitions
     :fields    [{:node-type :type-field :field-name "name" :type-name "String"}
                 {:node-type :type-field :field-name "age" :type-name "Int"}
                 {:node-type :type-field :field-name "picture" :type-name "Float"}]
     :kind      :OBJECT}]
   :operation-definitions
   [{:section        :operation-definitions
     :node-type      :operation-definition
     :operation-type {:type "query" :name "NullableValues"}
     :selection-set  [{:node-type     :field
                       :field-name    "person"
                       :arguments     [{:node-type :argument :argument-name "id" :value 4}]
                       :selection-set [{:node-type :field :field-name "id"}
                                       {:node-type :field :field-name "name"}
                                       {:node-type  :field
                                        :field-name "profilePic"
                                        :arguments  [{:node-type :argument :argument-name "width" :value 100}
                                                     {:node-type     :argument
                                                      :argument-name "height"
                                                      :value         50.0}]}]}]}]})

(def visited-document
  {:type-system-definitions
                         [{:node-type  :schema-definition
                           :query-type {:name "QueryRoot"}
                           :section    :type-system-definitions
                           :kind       :SCHEMA
                           :v/parentk  :children
                           :v/path     []}
                          {:node-type :type-definition
                           :type-name "QueryRoot"
                           :section   :type-system-definitions
                           :fields
                                      [{:node-type  :type-field
                                        :field-name "person"
                                        :type-name  "Person"
                                        :arguments
                                                    [{:node-type     :type-field-argument
                                                      :argument-name "id"
                                                      :type-name     "Int"
                                                      :v/parentk     :arguments
                                                      :v/path        ["Person" "id"]}]
                                        :v/parentk  :fields
                                        :v/path     ["Person"]}]
                           :kind      :OBJECT
                           :v/parentk :children
                           :v/path    []}
                          {:node-type :type-definition
                           :type-name "Person"
                           :section   :type-system-definitions
                           :fields
                                      [{:node-type  :type-field
                                        :field-name "name"
                                        :type-name  "String"
                                        :v/parentk  :fields
                                        :v/path     ["Person" "name"]}
                                       {:node-type  :type-field
                                        :field-name "age"
                                        :type-name  "Int"
                                        :v/parentk  :fields
                                        :v/path     ["Person" "age"]}
                                       {:node-type  :type-field
                                        :field-name "picture"
                                        :type-name  "Float"
                                        :v/parentk  :fields
                                        :v/path     ["Person" "picture"]}]
                           :kind      :OBJECT
                           :v/parentk :children
                           :v/path    ["Person"]}]
   :operation-definitions
                         [{:section        :operation-definitions
                           :node-type      :operation-definition
                           :operation-type {:type "query" :name "NullableValues"}
                           :selection-set
                                           [{:node-type  :field
                                             :field-name "person"
                                             :arguments
                                                         [{:node-type     :argument
                                                           :argument-name "id"
                                                           :value         4
                                                           :v/parentk     :arguments
                                                           :v/path        ["Person" "id"]}]
                                             :selection-set
                                                         [{:node-type  :field
                                                           :field-name "id"
                                                           :v/parentk  :selection-set
                                                           :v/path     ["Person" "id"]}
                                                          {:node-type  :field
                                                           :field-name "name"
                                                           :v/parentk  :selection-set
                                                           :v/path     ["Person" "name"]}
                                                          {:node-type  :field
                                                           :field-name "profilePic"
                                                           :arguments
                                                                       [{:node-type     :argument
                                                                         :argument-name "width"
                                                                         :value         100
                                                                         :v/parentk     :arguments
                                                                         :v/path        ["Person" "profilePic" "width"]}
                                                                        {:node-type     :argument
                                                                         :argument-name "height"
                                                                         :value         50.0
                                                                         :v/parentk     :arguments
                                                                         :v/path        ["Person" "profilePic" "height"]}]
                                                           :v/parentk  :selection-set
                                                           :v/path     ["Person" "profilePic"]}]
                                             :v/parentk  :selection-set
                                             :v/path     ["Person"]}]
                           :v/parentk      :children
                           :v/path         []}]
   :fragment-definitions nil})

(def visited-spec-document
  {:type-system-definitions
                         [{:node-type  :schema-definition
                           :query-type {:name "QueryRoot"}
                           :section    :type-system-definitions
                           :kind       :SCHEMA
                           :v/parentk  :children
                           :v/path     []}
                          {:node-type :type-definition
                           :type-name "QueryRoot"
                           :section   :type-system-definitions
                           :fields
                                      [{:node-type  :type-field
                                        :field-name "person"
                                        :type-name  "Person"
                                        :arguments
                                                    [{:node-type     :type-field-argument
                                                      :argument-name "id"
                                                      :type-name     "Int"
                                                      :v/parentk     :arguments
                                                      :v/path        ["Person" "id"]
                                                      :spec          :graphql-clj.-586053264.arg.Person/id}]
                                        :v/parentk  :fields
                                        :v/path     ["Person"]
                                        :spec       :graphql-clj.-586053264/Person}]
                           :kind      :OBJECT
                           :v/parentk :children
                           :v/path    []}
                          {:node-type :type-definition
                           :type-name "Person"
                           :section   :type-system-definitions
                           :fields
                                      [{:node-type  :type-field
                                        :field-name "name"
                                        :type-name  "String"
                                        :v/parentk  :fields
                                        :v/path     ["Person" "name"]
                                        :spec       :graphql-clj.-586053264.Person/name}
                                       {:node-type  :type-field
                                        :field-name "age"
                                        :type-name  "Int"
                                        :v/parentk  :fields
                                        :v/path     ["Person" "age"]
                                        :spec       :graphql-clj.-586053264.Person/age}
                                       {:node-type  :type-field
                                        :field-name "picture"
                                        :type-name  "Float"
                                        :v/parentk  :fields
                                        :v/path     ["Person" "picture"]
                                        :spec       :graphql-clj.-586053264.Person/picture}]
                           :kind      :OBJECT
                           :v/parentk :children
                           :v/path    ["Person"]
                           :spec      :graphql-clj.-586053264/Person}]
   :operation-definitions
                         [{:section        :operation-definitions
                           :node-type      :operation-definition
                           :operation-type {:type "query" :name "NullableValues"}
                           :selection-set
                                           [{:node-type  :field
                                             :field-name "person"
                                             :arguments
                                                         [{:node-type     :argument
                                                           :argument-name "id"
                                                           :value         4
                                                           :v/parentk     :arguments
                                                           :v/path        ["Person" "id"]
                                                           :spec          :graphql-clj.-586053264.arg.Person/id}]
                                             :selection-set
                                                         [{:node-type  :field
                                                           :field-name "id"
                                                           :v/parentk  :selection-set
                                                           :v/path     ["Person" "id"]
                                                           :spec       :graphql-clj.-586053264.Person/id}
                                                          {:node-type  :field
                                                           :field-name "name"
                                                           :v/parentk  :selection-set
                                                           :v/path     ["Person" "name"]
                                                           :spec       :graphql-clj.-586053264.Person/name}
                                                          {:node-type  :field
                                                           :field-name "profilePic"
                                                           :arguments
                                                                       [{:node-type     :argument
                                                                         :argument-name "width"
                                                                         :value         100
                                                                         :v/parentk     :arguments
                                                                         :v/path        ["Person" "profilePic" "width"]
                                                                         :spec          :graphql-clj.-586053264.arg.Person.profilePic/width}
                                                                        {:node-type     :argument
                                                                         :argument-name "height"
                                                                         :value         50.0
                                                                         :v/parentk     :arguments
                                                                         :v/path        ["Person" "profilePic" "height"]
                                                                         :spec          :graphql-clj.-586053264.arg.Person.profilePic/height}]
                                                           :v/parentk  :selection-set
                                                           :v/path     ["Person" "profilePic"]
                                                           :spec       :graphql-clj.-586053264.Person/profilePic}]
                                             :v/parentk  :selection-set
                                             :v/path     ["Person"]
                                             :spec       :graphql-clj.-586053264/Person}]
                           :v/parentk      :children
                           :v/path         []}]
   :fragment-definitions nil})

(deftest adding-path
  (testing "DFS traversal adding the path as we go"
    (is (= visited-document (:document (visitor/visit-document document [])))))
  (testing "pre-order visit and add a spec to relevant nodes"
    (is (= visited-spec-document (:document (visitor/visit-document document [spec/add-spec]))))))
