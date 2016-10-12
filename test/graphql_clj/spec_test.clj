(ns graphql-clj.spec-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [graphql-clj.spec :as spec]
            [graphql-clj.visitor :as visitor]
            [graphql-clj.visitor-test :as vt]))


(deftest named-spec
  (testing "base / default / scalar types"
    (is (= :graphql-clj/String (spec/named-spec "hash" ["String"]))))
  (testing "path with strings"
    (is (= :graphql-clj.hash.ns1.ns2/name) (spec/named-spec "hash" ["ns1" "ns2" "name"])))
  (testing "path with keywords"
    (is (= :graphql-clj.hash.ns1.ns2/name) (spec/named-spec 1234 [:ns1 :this/ns2 :graphql-clj/name]))))

(def visited (visitor/visit-document vt/document [spec/add-spec]))

(def schema-hash (-> visited :state :schema-hash))

(defn named-spec [path] (spec/named-spec schema-hash path))

(defn validate-path [path v] (s/valid? (named-spec path) v))

(deftest spec-validation
  (testing "pre-order visit and add a spec to relevant nodes"
    (is (validate-path ["Person" "name"] "Name"))
    (is (not (validate-path ["Person" "name"] 42)))
    (is (validate-path ["Person" "age"] 42))
    (is (not (validate-path ["Person" "age"] "42")))
    (is (validate-path ["Person" "picture"] 13.37))
    (is (not (validate-path ["Person" "picture"] "13.37")))
    (is (validate-path ["Person"] {:name "Name" :age 42 :picture 13.37}))
    (is (not (validate-path ["Person"] {:name 42 :age  "42" :picture "13.37"})))))
