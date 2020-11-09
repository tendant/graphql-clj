(ns graphql-clj.execution-test.basic-test
  (:use [clojure.test])
  (:require [graphql-clj.schema-parser :as schema-parser]
            [com.walmartlabs.lacinia.schema :as schema]
            [com.walmartlabs.lacinia :refer [execute]]
            [com.walmartlabs.lacinia.util :refer [attach-resolvers inject-resolvers]]))

(def sample-schema-1 "type Query {
  hero: Character
}

type Character {
  name: String
  friends: [Character]
  homeWorld: Planet
  species: Species
}

type Planet {
  name: String
  climate: String
}

type Species {
  name: String
  lifespan: Int
  origin: Planet
}")

(defn get-hero [context arguments value]
  (let [{:keys [episode]} arguments]
    (if (= episode :NEWHOPE)
      {:id 1000
       :name "Luke"
       :home_planet "Tatooine"
       :appears_in ["NEWHOPE" "EMPIRE" "JEDI"]}
      {:id 2000
       :name "Lando Calrissian"
       :home_planet "Socorro"
       :appears_in ["EMPIRE" "JEDI"]})))

(deftest test-sample-schema-1
  (testing "simple query"
    (let [s (-> sample-schema-1
                schema-parser/parse-schema
                (inject-resolvers {:Query/hero (fn [& opts]
                                                 {:name "Test Name 1"})
                                   :Character/friends (fn [& opts]
                                                        [{:name "Test Friend 1"}])})
                schema/compile)
          _ (println "s: " s)
          query "{ hero { name friends { name } } }"]
      (is (= (execute s query nil nil)
             {:data
              {:hero {:name "Test Name 1", :friends [{:name "Test Friend 1"}]}}})))))
