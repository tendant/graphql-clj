(ns graphql-clj.query-validator-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [graphql-clj.parser :as parser]
            [graphql-clj.schema-validator :as schema-validator]
            [graphql-clj.query-validator :as query-validator]))

(def ^:private example-schema
  (schema-validator/validate-schema "enum DogCommand { SIT, DOWN, HEEL }

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

type Arguments {
  multipleReqs(x: Int!, y: Int!): Int!
  booleanArgField(booleanArg: Boolean): Boolean
  floatArgField(floatArg: Float): Float
  intArgField(intArg: Int): Int
  nonNullBooleanArgField(nonNullBooleanArg: Boolean!): Boolean!
  nonNullListOfBooleanField(nonNullListOfBooleanArg: [Boolean]!) : Boolean!
  listOfNonNullBooleanField(listOfNonNullBooleanArg: [Boolean!]) : Boolean!
  nonNullListOfNonNullBooleanField(nonNullListOfNonNullBooleanArg: [Boolean!]!) : Boolean!
  booleanListArgField(booleanListArg: [Boolean]!): [Boolean]
}

type QueryRoot {
  dog: Dog
  arguments: Arguments
}

type MutationRoot {
  dog: Dog
  arguments: Arguments
}

schema {
  query: QueryRoot
  mutation: MutationRoot
}"))

(def ^:private query-only-schema
  (schema-validator/validate-schema "type QueryRoot { name : String }"))

(defn- trace-element [src sl sc si el ec ei]
  {:source src :start {:line sl :column sc :index si} :end {:line el :column ec :index ei}})

(defn- err [msg sl sc si el ec ei & trace]
  (let [e {:message msg :start {:line sl :column sc :index si} :end {:line el :column ec :index ei}}]
    (if (empty? trace) e (assoc e :trace (into [] trace)))))

(defmacro deftest-valid [name schema query expected]
  `(deftest ~name
     (let [expect# ~expected
           [errors# actual#] (->> (query-validator/validate-query ~schema ~query))]
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
  (err "operation with name 'dogOperation' is already declared" 7 13 69 7 25 81)
  (err "field 'mutateDog' is not defined on type 'MutationRoot'" 8 6 89 8 15 98))

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
  (err "fragment cycle detected: nameFrag -> barkVolumeFrag -> ..." 14 9 251 14 17 259
       {:source "fragment spread 'barkVolumeFrag'", :start {:line 10, :column 6, :index 168}, :end {:line 10, :column 23, :index 185}}
       {:source "fragment spread 'nameFrag'", :start {:line 1, :column 9, :index 8}, :end {:line 1, :column 20, :index 19}}))

(deftest-invalid fragment-spreads-must-not-form-cycles-2 example-schema
  "{ dog { ...a } }
   fragment a on Dog { name ...b }
   fragment b on Dog { name ...c }
   fragment c on Dog { name ...a }"
  (err "fragment cycle detected: a -> b -> c -> ..." 4 32 118 4 33 119
       {:source "fragment spread 'c'", :start {:line 3, :column 29, :index 80}, :end {:line 3, :column 33, :index 84}}
       {:source "fragment spread 'b'", :start {:line 2, :column 29, :index 45}, :end {:line 2, :column 33, :index 49}}
       {:source "fragment spread 'a'", :start {:line 1, :column 9, :index 8}, :end {:line 1, :column 13, :index 12}}))

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

;; 5.7.1
(deftest-invalid variable-uniqueness example-schema
  "query houseTrainedQuery($atOtherHomes: Boolean, $atOtherHomes: Boolean) {
     dog {
       isHousetrained(atOtherHomes: $atOtherHomes)
     }
   }"
  (err "variable '$atOtherHomes' is already declared" 1 49 48 1 71 70))

;; 5.7.2
(deftest-invalid variable-required-with-default-value example-schema
  "query houseTrainedQuery($atOtherHomes: Boolean! = true) {
     dog {
       isHousetrained(atOtherHomes: $atOtherHomes)
     }
   }"
  (err "variable '$atOtherHomes' is required, and thus cannot have a default value" 1 25 24 1 55 54))

;; The parser allows this variable default values to be variables,
;; though it could be made to not allow it.  Instead of addressing it
;; in the parser though, we check in validation.
(deftest-invalid variable-default-value-cannot-be-another-variable example-schema
  "query houseTrainedQuery($a:Boolean = true, $b:Boolean = $a) {
     a : dog {
       isHousetrained(atOtherHomes: $a)
     }
     b : dog {
       isHousetrained(atOtherHomes: $b)
     }
   }"
  (err "variable default value must be constant" 1 57 56 1 59 58))

;; 5.7.2
(deftest-invalid variable-type-does-not-match-default-value example-schema
  "query houseTrainedQuery($atOtherHomes: Boolean = \"true\") {
     dog {
       isHousetrained(atOtherHomes: $atOtherHomes)
     }
   }"
  (err "variable '$atOtherHomes' has a default value that cannot be coersed to its declared type" 1 25 24 1 56 55))

(deftest-valid variable-coearse-int-to-float example-schema
  "query intToFloatQuery($floatVar: Float = 1) {
     arguments {
       floatArgField(floatArg: $floatVar)
     }
  }"
  nil)

(deftest-invalid variable-type-is-not-defined example-schema
  "query takesMouse($mouse: Mouse) {
   }"
  (err "variable '$mouse' type 'Mouse' is not defined" 1 26 25 1 31 30)
  (err "variable '$mouse' is not used" 1 19 18 1 24 23))

;; 5.7.3
(deftest-invalid variable-must-be-input-type-1 example-schema
  "query takesCat($cat: Cat) {
   }"
  (err "variable '$cat' type 'Cat' is not a valid input type" 1 16 15 1 25 24)
  (err "variable '$cat' is not used" 1 17 16 1 20 19))

;; 5.7.3
(deftest-invalid variable-must-be-input-type-2 example-schema
  "query takesDogBang($dog: Dog!) {
   }"
  (err "variable '$dog' type 'Dog!' is not a valid input type" 1 20 19 1 30 29)
  (err "variable '$dog' is not used" 1 21 20 1 24 23))

;; 5.7.3
(deftest-invalid variable-must-be-input-type-3 example-schema
  "query takesListOfPet($pets: [Pet]) {
   }"
  (err "variable '$pets' type '[Pet]' is not a valid input type" 1 22 21 1 34 33)
  (err "variable '$pets' is not used" 1 23 22 1 27 26))

;; 5.7.3
(deftest-invalid variable-must-be-input-type-4 example-schema
  "query takesCatOrDog($catOrDog: CatOrDog) {
   }"
  (err "variable '$catOrDog' type 'CatOrDog' is not a valid input type" 1 21 20 1 40 39)
  (err "variable '$catOrDog' is not used" 1 22 21 1 30 29))

;; TODO: check that variable types [Boolean!], ComplexInput are valid

;; 5.7.4
(deftest-invalid variable-undefined example-schema
  "query variableIsNotDefined {
     dog {
       isHousetrained(atOtherHomes: $atOtherHomes)
     }
   }"
  (err "variable '$atOtherHomes' is not defined" 3 37 76 3 50 89))


;; 5.7.5 All Variables Used
(deftest-invalid variable-unused example-schema
  "query variableUnused($atOtherHomes: Boolean) {
     dog {
       isHousetrained
     }
   }"
  (err "variable '$atOtherHomes' is not used" 1 23 22 1 35 34))


;; 5.7.5 All Variables Used
;; 5.7.4
(deftest-valid fragments-complicate-variable-rules example-schema
  "query variableIsDefinedUsedInSingleFragment($atOtherHomes: Boolean) {
     dog {
       ...isHousetrainedFragment
     }
   }

   fragment isHousetrainedFragment on Dog {
     isHousetrained(atOtherHomes: $atOtherHomes)
   }"
  ;; valid since $atOtherHomes is used by fragment
  nil)

(deftest-invalid fragments-used-in-query-that-does-not-define-variable example-schema
  "query variableIsNotDefinedUsedInSingleFragment {
     dog {
       ...isHousetrainedFragment
     }
   }

   fragment isHousetrainedFragment on Dog {
     isHousetrained(atOtherHomes: $atOtherHomes)
   }"
  (err "variable '$atOtherHomes' is not defined" 8 35 184 8 48 197
       (trace-element "fragment spread 'isHousetrainedFragment'" 3 8 67 3 33 92)))

(deftest-invalid twice-nested-fragment-variable-not-defined example-schema
  "query variableIsNotDefinedUsedInNestedFragment {
     dog {
       ...outerHousetrainedFragment
     }
   }

   fragment outerHousetrainedFragment on Dog {
     ...isHousetrainedFragment
   }

   fragment isHousetrainedFragment on Dog {
     isHousetrained(atOtherHomes: $atOtherHomes)
   }"
  (err "variable '$atOtherHomes' is not defined" 12 35 271 12 48 284
       (trace-element "fragment spread 'isHousetrainedFragment'" 8 6 161 8 31 186)
       (trace-element "fragment spread 'outerHousetrainedFragment'" 3 8 67 3 36 95)))

(deftest-valid twice-nested-fragment-variable-defined example-schema
  "query variableIsNotDefinedUsedInNestedFragment($atOtherHomes:Boolean) {
     dog {
       ...outerHousetrainedFragment
     }
   }

   fragment outerHousetrainedFragment on Dog {
     ...isHousetrainedFragment
   }

   fragment isHousetrainedFragment on Dog {
     isHousetrained(atOtherHomes: $atOtherHomes)
   }"
  nil)

(deftest-valid variables-referenced-in-all-referencing-fragments example-schema
  "query housetrainedQueryOne($atOtherHomes: Boolean) {
     dog {
       ...isHousetrainedFragment
     }
   }

   query housetrainedQueryTwo($atOtherHomes: Boolean) {
     dog {
       ...isHousetrainedFragment
     }
   }

   fragment isHousetrainedFragment on Dog {
     isHousetrained(atOtherHomes: $atOtherHomes)
   }"
  nil)

(deftest-invalid variable-not-defined-in-one-of-two-fragments example-schema
  "query housetrainedQueryOne($atOtherHomes: Boolean) {
     dog {
       ...isHousetrainedFragment
     }
   }

   query housetrainedQueryTwoNotDefined {
     dog {
       ...isHousetrainedFragment
     }
   }

   fragment isHousetrainedFragment on Dog {
     isHousetrained(atOtherHomes: $atOtherHomes)
   }"
  ;; TODO: this is really important to include the :trace 
  (err "variable '$atOtherHomes' is not defined" 14 35 287 14 48 300
       (trace-element "fragment spread 'isHousetrainedFragment'" 9 8 170 9 33 195)))


(deftest-invalid one-of-two-variables-not-used example-schema
  "query queryWithUsedVar($atOtherHomes: Boolean) {
     dog {
       ...isHousetrainedFragment
     }
   }

   query queryWithExtraVar($atOtherHomes: Boolean, $extra: Int) {
     dog {
       ...isHousetrainedFragment
     }
   }

   fragment isHousetrainedFragment on Dog {
     isHousetrained(atOtherHomes: $atOtherHomes)
   }"
  ;; no error on atOtherHomes since it was referenced
  (err "variable '$extra' is not used" 7 53 158 7 58 163))

;; 5.7.6 All Variable Usages are Allowed
(deftest-invalid int-variable-to-boolean-parameter example-schema
  "query intCannotGoIntoBoolean($intArg: Int) {
     arguments {
       booleanArgField(booleanArg: $intArg)
     }
   }"
  (err "argument type mismatch: 'booleanArg' expects type 'Boolean', argument '$intArg' is type 'Int'" 3 36 97 3 43 104))


(deftest-invalid list-variable-to-non-list-parameter example-schema
  "query booleanListCannotGoIntoBoolean($booleanListArg: [Boolean]) {
     arguments {
       booleanArgField(booleanArg: $booleanListArg)
     }
   }"
  (err "argument type mismatch: 'booleanArg' expects type 'Boolean', argument '$booleanListArg' is type '[Boolean]'" 3 36 119 3 51 134))

(deftest-invalid nullable-variable-to-non-null-parameter example-schema
  "query booleanArgQuery($booleanArg: Boolean) {
     arguments {
       nonNullBooleanArgField(nonNullBooleanArg: $booleanArg)
     }
   }"
  ;; TODO: can we describe this error more clearly, the '!' might be hard to notice
  (err "argument type mismatch: 'nonNullBooleanArg' expects type 'Boolean!', argument '$booleanArg' is type 'Boolean'" 3 50 112 3 61 123))

(deftest-valid default-variable-to-non-null-parameter-is-valid example-schema
  "query booleanArgQueryWithDefault($booleanArg: Boolean = true) {
     arguments {
       nonNullBooleanArgField(nonNullBooleanArg: $booleanArg)
     }
   }"
  nil)

(deftest-valid non-null-variable-to-null-parameter-is-valid example-schema
  "query nonNullListToList($nonNullBooleanList: [Boolean]!) {
     arguments {
       booleanListArgField(booleanListArg: $nonNullBooleanList)
     }
   }"
  nil)

;; [T] cannot be passed to [T]!
(deftest-invalid nullable-list-variable-to-non-null-list-parameter example-schema
  "query listToNonNullList($booleanList: [Boolean]) {
     arguments {
       nonNullListOfBooleanField(nonNullListOfBooleanArg: $booleanList)
     }
   }"
  (err "argument type mismatch: 'nonNullListOfBooleanArg' expects type '[Boolean]!', argument '$booleanList' is type '[Boolean]'"
       3 59 126 3 71 138))

;; [T] cannot be passed to [T!]
(deftest-invalid list-of-nullable-to-list-of-non-nullable-parameter example-schema
  "query listToNonNullList($booleanList: [Boolean]) {
     arguments {
       listOfNonNullBooleanField(listOfNonNullBooleanArg: $booleanList)
     }
   }"
  (err "argument type mismatch: 'listOfNonNullBooleanArg' expects type '[Boolean!]', argument '$booleanList' is type '[Boolean]'"
       3 59 126 3 71 138))

;; [T] cannot be passed to [T!]!
(deftest-invalid list-of-nullable-to-non-null-list-of-non-null-parameter example-schema
  "query listToNonNullList($booleanList: [Boolean]) {
     arguments {
       nonNullListOfNonNullBooleanField(nonNullListOfNonNullBooleanArg: $booleanList)
     }
   }"
  (err "argument type mismatch: 'nonNullListOfNonNullBooleanArg' expects type '[Boolean!]!', argument '$booleanList' is type '[Boolean]'"
       3 73 140 3 85 152))

;; [T]! cannot be passed to [T!]!
(deftest-invalid non-null-list-of-nullable-to-non-null-list-of-non-null-parameter example-schema
  "query listToNonNullList($booleanList: [Boolean]!) {
     arguments {
       nonNullListOfNonNullBooleanField(nonNullListOfNonNullBooleanArg: $booleanList)
     }
   }"
  (err "argument type mismatch: 'nonNullListOfNonNullBooleanArg' expects type '[Boolean!]!', argument '$booleanList' is type '[Boolean]!'"
       3 73 141 3 85 153))

;; 5.3.1
(deftest-valid arg-on-required-arg example-schema
  "fragment argOnRequiredArg on Dog {
     doesKnowCommand(dogCommand: SIT)
   }

   fragment argOnOptional on Dog {
     isHousetrained(atOtherHomes: true) @include(if: true)
   }"
  nil)

;; 5.3.1
(deftest-invalid invalid-arg-name example-schema
  "{                          dog {
     doesKnowCommand(command: CLEAN_UP_HOUSE)
   } }"
  (err "argument 'command' is not defined on 'doesKnowCommand'" 2 31 63 2 45 77)
  (err "required argument 'dogCommand' is missing" 2 6 38 2 46 78))


;; 5.3.2
(deftest-invalid argument-uniqueness example-schema
  "fragment duplicateArgName on Dog {
     doesKnowCommand(dogCommand: SIT, dogCommand: DOWN)
   }"
  (err "argument 'dogCommand' already has a value" 2 39 73 2 49 83))

;; 5.3.3.1
(deftest-valid argument-boolean-literal example-schema
  "fragment goodBooleanArg on Arguments {
     booleanArgField(booleanArg: true)
   }"
  nil)

;; 5.3.3.1
(deftest-valid argument-int-literal-to-float-argument example-schema
  "fragment coercedIntIntoFloatArg on Arguments {
     floatArgField(floatArg: 1)
   }"
  nil)

;; 5.3.3.1
(deftest-invalid argument-string-literal-to-float-invalid example-schema
  "fragment stringIntoInt on Arguments {
     intArgField(intArg: \"3\")
  }"
  (err "argument type mismatch: 'intArg' expects type 'Int', argument is type 'String'" 2 26 63 2 29 66))

;; 5.3.3.2
(deftest-valid valid-boolean-arguments example-schema
  "fragment goodBooleanArg on Arguments {
     booleanArgField(booleanArg: true)
   }

   fragment goodNonNullArg on Arguments {
     nonNullBooleanArgField(nonNullBooleanArg: true)
   }"
  nil)

;; 5.3.3.2
(deftest-valid boolean-default-arg example-schema
  "fragment goodBooleanArgDefault on Arguments {
     booleanArgField
   }"
  nil)

;; 5.3.3.2
(deftest-invalid missing-required-arg example-schema
  "fragment missingRequiredArg on Arguments {
     nonNullBooleanArgField
   }"
  (err "required argument 'nonNullBooleanArg' is missing" 2 6 48 2 28 70))

(deftest-invalid required-arg-cannot-be-null example-schema
  "fragment missingRequiredArg on Arguments {
     nonNullBooleanArgField(nonNullBooleanArg: null)
   }"
  (err "required argument 'nonNullBooleanArg' is null" 2 48 90 2 52 94))

;; TODO:
;; Add test that:
;;
;; "mutation dogOperation {
;;    mutateDog {
;;      id
;;    }
;;  }"
;;
;; Does NOT throw an exception when mutation root is not defined.  (Currently happening due to undefined type)
(deftest-invalid missing-mutation-root query-only-schema
  "mutation dogOperation {
     mutateDog { id }
   }"
  (err "schema does not define a root 'mutation' type" 1 1 0 3 5 50))


(deftest-valid test-introspection-schema example-schema
  "query IntrospectionQuery {
    __schema {
      types {
        ...FullType
      }
    }
  }
  fragment FullType on __Type {
    kind
    name
  }"
  [{:tag :query-definition,
    :name 'IntrospectionQuery,
    :selection-set
    [{:tag :selection-field,
      :name '__schema,
      :resolved-type {:tag :basic-type, :name '__Schema, :required true}
      :selection-set
      [{:tag :selection-field,
        :name 'types,
        :resolved-type {:tag :list-type, :inner-type {:tag :basic-type, :name '__Type, :required true}, :required true},
        :selection-set
        [{:tag :selection-field,
          :name 'kind,
          :resolved-type {:tag :basic-type, :name '__TypeKind, :required true}}
         {:tag :selection-field,
          :name 'name,
          :resolved-type {:tag :basic-type, :name 'String}}]}]}]}])

(def ^:private input-object-test-schema
  (-> "schema { query: Query }
       input TextInput { value: String }
       input WorldInput { text: TextInput }
       type Query { hello(world: WorldInput): String }"
      parser/parse-schema
      schema-validator/validate-schema))

(deftest-valid test-input-object input-object-test-schema
  "{ hello(world: {text: \"World\"}) }"
  [{:selection-set [{:tag :selection-field, :name 'hello, :arguments [{:tag :argument, :name 'world, :value {:tag :object-value, :fields [{:tag :object-field, :name 'text, :value {:tag :string-value, :image "\"World\"", :value "World"}}]}}], :resolved-type {:tag :basic-type, :name 'String}}], :tag :selection-set}])


(deftest-valid test-list-literal-argument example-schema
  "mutation {
     arguments {
       listOfNonNullBooleanField(listOfNonNullBooleanArg:[])
     }
   }"
  nil)

;; ======================================================================
;; Additional validations to implement follow in the comment block below
(comment 
;; 5.3.1
(deftest-invalid invalid-arg-name-on-directive example-schema
  "fragment invalidArgName on Dog {
     isHousetrained(atOtherHomes: true) @include(unless: false)
   }"
  (err "argument 'unless' is not defined on '@include'" 0 0 0 0 0 0))

  
)
