(ns graphql-clj.query-validator-test
  (:require [clojure.test :refer :all]
            [graphql-clj.parser :as parser]
            [graphql-clj.schema-validator :as schema-validator]
            [graphql-clj.query-validator :as query-validator]))

(def ^:private example-schema
  (->> "enum DogCommand { SIT, DOWN, HEEL }

type Dog implements Pet {
  name: String!
  nickname: String
  barkVolume: Int
  doesKnowCommand(dogCommand: DogCommand!): Boolean!
  isHousetrained(atOtherHomes: Boolean): Boolean!
  owner: Human
}

interface Sentient {
  name: String!
}

interface Pet {
  name: String!
}

type Alien implements Sentient {
  name: String!
  homePlanet: String
}

type Human implements Sentient {
  name: String!
}

enum CatCommand { JUMP }

type Cat implements Pet {
  name: String!
  nickname: String
  doesKnowCommand(catCommand: CatCommand!): Boolean!
  meowVolume: Int
}

union CatOrDog = Cat | Dog
union DogOrHuman = Dog | Human
union HumanOrAlien = Human | Alien

type QueryRoot {
  dog: Dog
}"
       (parser/parse-schema)
       (schema-validator/validate-schema)
       peek))

(defn- err [msg sl sc si el ec ei]
  {:message msg :start {:line sl :column sc :index si} :end {:line el :column ec :index ei}})

(defmacro deftest-valid [name schema query expected]
  `(deftest ~name
     (let [expect# ~expected
           [errors# actual#] (->> (parser/parse-query-document ~query)
                                 (query-validator/validate-query ~schema))]
       (if (= expect# actual#)
         (report {:type :pass})
         (report {:type :fail :expected expect# :actual actual#})))))

(defmacro deftest-invalid [name schema query & errors]
  `(deftest ~name
     (let [[errors# actual#] (->> (parser/parse-query-document ~query)
                                (query-validator/validate-query ~schema))]
       (is (= ~(vec errors) errors#)))))
  

(deftest-valid sec-5-1-1-1-operation-name-uniqueness-valid example-schema
  "query getDogName {
     dog {
       name
     }
   }"
  [{:tag :query-definition
    :name 'getDogName
    :selection-set
    [{:tag :selection-field
      :name 'dog
      :resolved-type {:tag :basic-type, :name 'Dog}
      :selection-set
      [{:tag :selection-field
        :name 'name
        :resolved-type {:tag :basic-type, :name 'String, :required true}}]}]}])

(deftest-valid sec-5-1-1-1-operation-name-uniqueness-valid-2 example-schema
  "query getOwnerName {
     dog {
       owner {
         name
       }
     }
   }"
  [{:tag :query-definition
    :name 'getOwnerName
    :selection-set
    [{:tag :selection-field
      :name 'dog
      :resolved-type {:tag :basic-type, :name 'Dog}
      :selection-set
      [{:tag :selection-field
        :name 'owner
        :resolved-type {:tag :basic-type, :name 'Human}
        :selection-set
        [{:tag :selection-field, :name 'name,
          :resolved-type {:tag :basic-type, :name 'String, :required true}}]}]}]}])

(deftest-invalid sec-5-1-1-1-operation-name-uniqueness-invalid-1 example-schema
  "query getName {
     dog {
       name
     }
   }

   query getName {
     dog {
       owner {
         name
       }
     }
   }"
  (err "operation with name 'getName' is already declared" 7 10 61 7 17 68))

(deftest-invalid sec-5-1-1-1-operation-name-uniqueness-invalid-2 example-schema
  "query dogOperation {
     dog {
       name
     }
   }

   mutation dogOperation {
     mutateDog {
       id
     }
   }"
  (err "operation with name 'dogOperation' is already declared" 7 13 69 7 25 81))

(deftest-invalid sec-5-1-2-1-lone-anonymous-operation-invalid example-schema
  "{
  dog {
    name
  }
}

query getName {
  dog {
    owner {
      name
    }
  }
}"
  (err "anonymous selection set must be lone operation" 1 1 0 5 2 24))


(deftest-invalid sec-5-1-2-1-lone-anonymous-operation-invalid-2 example-schema
  "{ dog { name } } { dog { owner { name } } }"
  (err "anonymous selection set is already declared" 1 18 17 1 44 43))

;; Check that :type is added to dog field
(deftest-valid sec-5-2-1-field-selection-on-objects-interfaces-and-union-types-valid example-schema
  "{ dog }"
  [{:tag :selection-set
    :selection-set [{:tag :selection-field
                     :name 'dog
                     ;; the :resolve-type field is associated to the query
                     ;; during validation.
                     :resolved-type {:tag :basic-type :name 'Dog}}]}])

(deftest-valid sec-5-1-2-1-lone-anonymous-operation-valid example-schema
  "{
    dog {
      name
    }
  }"
  ;; this checks also that fields are recursively checked and
  ;; that :resolved-type fields are associated.
  [{:tag :selection-set
    :selection-set
    [{:tag :selection-field
      :name 'dog
      :resolved-type {:tag :basic-type :name 'Dog}
      :selection-set
      [{:tag :selection-field
        :name 'name
        :resolved-type
        {:tag :basic-type
         :name 'String
         :required true}}]}]}])

(deftest-invalid sec-5-2-1-field-selection-on-objects-interfaces-and-union-types-1 example-schema
  "{ cat }"
  (err "field 'cat' is not defined on type 'QueryRoot'" 1 3 2 1 6 5))

(deftest-invalid sec-5-2-1-field-selection-on-objects-interfaces-and-union-types-1b example-schema
  "query getCat { cat }"
  (err "field 'cat' is not defined on type 'QueryRoot'" 1 16 15 1 19 18))

(deftest-invalid sec-5-2-1-field-selection-on-objects-interfaces-and-union-types-2 example-schema
  "{ dog { meowVolume } }"
  (err "field 'meowVolume' is not defined on type 'Dog'" 1 9 8 1 19 18))

(deftest-invalid sec-5-2-1-field-selection-on-objects-interfaces-and-union-types-3 example-schema
  "{ dog: cat }"
  (err "field 'cat' (aliased as 'dog') is not defined on type 'QueryRoot'" 1 8 7 1 11 10))

;; TODO: 5.2.1


;; 5.4.1.1 Fragment name uniqueness
(deftest-invalid fragment-name-uniqueness example-schema
  "{ dog { ...frag1 } }
   fragment frag1 on Dog { name }
   fragment frag1 on Dog { name }"
  (err "fragment 'frag1' is already declared" 3 13 67 3 18 72))

(deftest-invalid fragment-on-undefined-type example-schema
  "{ dog { ...nameFrag } }
   fragment nameFrag on Foo {
     name
   }"
  (err "fragment on undefined type 'Foo'" 2 25 48 2 28 51))

;; 5.4.1.3 Fragments on composite types
(deftest-invalid fragment-on-composite-type-1 example-schema
  "{ dog }
   fragment fragOnScalar on Int {
     something
   }"
  (err "fragment on non-composite type 'Int'" 2 29 36 2 32 39))

;; 5.4.1.3 Fragments on composite types
(deftest-invalid fragment-on-composite-type-2 example-schema
  "fragment inlineFragmentOnScalar on Dog {
     ... on Boolean {
       somethingElse
     }
   }"
  (err "inline fragment on non-composite type 'Boolean'" 2 13 53 2 20 60))

(deftest-invalid inline-fragment-on-undeclared-type example-schema
  "fragment inlineFragmentOnScalar on Dog {
     ... on UndeclaredType {
       somethingElse
     }
   }"
  (err "inline fragment on undefined type 'UndeclaredType'" 2 13 53 2 27 67))

;; 5.4.2.2 Fragment spreads must not form cycles
(deftest-invalid fragment-spreads-must-not-form-cycles example-schema
  "{ dog { ...nameFrag } }
   fragment otherFrag on Dog {
     name
   }
   fragment extraFrag on Dog {
     ...nameFrag
   }
   fragment nameFrag on Dog {
     name
     ...barkVolumeFrag
   }
   fragment barkVolumeFrag on Dog {
     barkVolume
     ...nameFrag
   }"
  (err "fragment 'nameFrag' contains a cyclic reference" 8 13 135 8 21 143)
  (err "fragment 'barkVolumeFrag' contains a cyclic reference" 12 13 203 12 27 217))

(deftest-invalid fragment-spreads-must-not-form-cycles-2 example-schema
  "{ dog { ...a } }
   fragment a on Dog { name ...b }
   fragment b on Dog { name ...c }
   fragment c on Dog { name ...a }"
  (err "fragment 'a' contains a cyclic reference" 2 13 29 2 14 30)
  (err "fragment 'b' contains a cyclic reference" 3 13 64 3 14 65)
  (err "fragment 'c' contains a cyclic reference" 4 13 99 4 14 100))

(deftest-invalid fragment-undefined example-schema
  "{ dog { ...nameFrag } }
   fragment nameFrag on Dog {
     ... undefinedFrag
   }"
  (err "fragment 'undefinedFrag' is not defined" 3 10 63 3 23 76))

(deftest-invalid sec-5-2-1-fragment-field-not-defined example-schema
  "{ dog }
   fragment fieldNotDefined on Dog {
     meowVolume
   }"
  (err "field 'meowVolume' is not defined on type 'Dog'" 3 6 50 3 16 60))

;; TODO
;; ;; 5.4.1.4 Fragments must be used
;; (deftest-invalid fragment-must-be-used
;;   "fragment nameFragment on Dog { # unused
;;      name
;;    }
;;    {
;;      dog {
;;        name
;;      }
;;    }"
;;   {})

;; TODO: 5.4.2.3 fragment spread is possible
;; TODO: 5.5.1 input object field uniqueness
;; TODO: 5.6 directives
;; TODO: 5.7 variables

