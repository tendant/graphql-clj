(ns graphql-clj.box-test
  (:require [clojure.test :refer :all]
            [graphql-clj.box :as box]))

(deftest boxed-values
  (let [bv (box/->Box 1234 {:this "that"})
        bv2 (box/->Box 1234 {:that "this"})
        bv3 (box/->Box {:a "b"} {:c "d"})
        bv4 (box/->Box {:a "b"} {:e "f"})
        bv5 (box/->Box bv {:abc 123})]
    (testing "we can create and manipulate boxed values"
      (is (= bv 1234))                                      ;; we preserve value equality for a boxed value and it's unboxed equivalent
      (is (= bv bv2))                                       ;; we preserve value equality for two objects
      (is (= @bv 1234))                                     ;; we can deref a boxed value to get the original unboxed value
      (is (= @bv @bv2))                                     ;; after derefing two boxed values, they are still equal
      (is (= (meta bv) {:this "that"}))                     ;; we have metadata preserved, even for Clojure types that don't
      (is (= (str bv) "1234"))                              ;; we can cast to string
      (is (= (.toString bv2) "1234"))
      (is (= 1234 bv))                                      ;; we preserve value equality in either order for simple types
      (is (= bv3 {:a "b"}))                                 ;; we preserve value equality in Box-first order for complex types
      (is (= bv3 bv4))
      (is (= (meta bv5) {:abc 123}))                        ;; we behave in a sane manner for nested boxed values
      (is (= (name bv5) "1234"))
      (is (= (str bv5) "1234"))
      (is (= @bv5 1234)))
    (testing "however, the abstraction leaks - behavior differs from underlying values" ;; so we don't want this utility type leaking out of the parsing / validation phase
      (is (not= {:a "b"} bv3)))))
