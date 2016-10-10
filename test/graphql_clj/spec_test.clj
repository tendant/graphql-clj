(ns graphql-clj.spec-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [graphql-clj.spec :as spec]
            [graphql-clj.visitor :as visitor]
            [graphql-clj.visitor-test :as vt]))

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
    (is (= (s/describe :graphql-clj.spec/Person) '(keys :opt (:graphql-clj.spec/Person-name
                                                               :graphql-clj.spec/Person-age
                                                               :graphql-clj.spec/Person-picture))))
    (is (not (s/valid? :graphql-clj.spec/Person {:graphql-clj.spec/Person-name 42
                                                 :graphql-clj.spec/Person-age  "42"
                                                 :graphql-clj.spec/Person-picture "13.37"})))))
