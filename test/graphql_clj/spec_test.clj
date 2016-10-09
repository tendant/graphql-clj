(ns graphql-clj.spec-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [clojure.data]
            [graphql-clj.spec :as spec]
            [graphql-clj.visitor-test :as vt]
            [graphql-clj.visitor :as visitor]))

(visitor/visit-document vt/document [spec/add-spec])

(deftest spec-validation
  (testing "pre-order visit and add a spec to relevant nodes"
    (is (s/valid? :graphql-clj.spec/Person-name "Name"))
    (is (not (s/valid? :graphql-clj.spec/Person-name 42)))
    (is (s/valid? :graphql-clj.spec/Person-age 42))
    (is (not (s/valid? :graphql-clj.spec/Person-age "42")))
    (is (s/valid? :graphql-clj.spec/Person-picture 13.37))
    (is (not (s/valid? :graphql-clj.spec/Person-picture "13.37")))
    (is (s/valid? :graphql-clj.spec/Person {"name" "Name" "age" 42 "picture" 13.37}))
    (is (not (s/valid? :graphql-clj.spec/Person {:graphql-clj.spec/Person-name 42
                                                 :graphql-clj.spec/Person.age  "42"
                                                 :graphql-clj.spec/Person-picture "13.37"})))))
