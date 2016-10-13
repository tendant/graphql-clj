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

(defn get-person-arg [document]
  (-> document :document :type-system-definitions (nth 1)  :fields first :arguments first))

(deftest adding-path
  (testing "DFS traversal adding the path as we go"
    (is (= ["Person" "id"]
           (:v/path (get-person-arg (visitor/visit-document document []))))))

  (testing "pre-order visit and add a spec to relevant nodes"
    (is (= :graphql-clj.-586053264.arg.Person/id
           (:spec (get-person-arg (visitor/visit-document document [spec/add-spec])))))))
