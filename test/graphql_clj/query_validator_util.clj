(ns graphql-clj.query-validator-util
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [graphql-clj.schema-validator :as schema-validator]
            [graphql-clj.query-validator :as query-validator]))

(defmacro deftest-valid [name schema query expected]
  `(deftest ~name
     (let [expect# ~expected
           [errors# actual#] (query-validator/validate-query ~schema ~query)]
       (if (empty? errors#)
         (report {:type :pass})
         (report {:type :fail :expected [] :actual errors#}))
       (if (or (nil? expect#) (= expect# actual#))
         (report {:type :pass})
         (report {:type :fail :expected expect# :actual actual#})))))

(defmacro deftest-invalid [name schema query & errors]
  `(deftest ~name
     (let [[errors# actual#] (query-validator/validate-query ~schema ~query)
           expect# ~(vec errors)]
       (if (= expect# errors#)
         (report {:type :pass})
         (report {:type :fail :expected expect# :actual errors#})))))
