(ns graphql-clj.parser
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]
            [graphql-clj.type :as type]))

(log/merge-config! {:level :info
                    :appenders {:println {:async? false}}})

(def whitespace
  (insta/parser
    "whitespace = #'\\s+'"))

(def ^{:private true} parser- (insta/parser (io/resource "graphql.bnf")))

(defn parse
  [stmt]
  (log/debug stmt)
  (time (parser- stmt)))

(def transformation-map
  {:OperationDefinition (fn operation-definition [& args]
                          (log/debug "operation-definition: " args)
                          (log/debug "start")
                          (log/debug (map (fn [a] (log/debug "new: ")
                                          (if (> (count a) 2)
                                            (log/debug "1" (first a) "2" (second a) "3" (nth a 2) "4" (nth a 3))) (count a))
                                        args))
                          (let [definition (into {:operation-type "query"} args)]
                            (log/debug "good")
                            [:operation-definition definition]))
   :OperationType (fn operation-type [type]
                    (log/debug "operation-type: " type)
                    [:operation-type type])
   :Definition (fn definition [& args]
                 (log/debug "definition: " args)
                 (let [operation-definition (first (filter #(= :operation-definition (first %)) args)) ; FIXME: assume there is only one operation-definition
                       fragment-definitions (filter #(= :fragment-definition (first %)) args)
                       fragments (reduce (fn reduce-fragments [v fragment]
                                           (let [props (second fragment)
                                                 name (:fragment-name props)]
                                             (assoc v name props)))
                                         {} fragment-definitions)]
                   {:operation-definition (second operation-definition)
                    :fragments fragments}))
   :Document (fn document [& args]
               (log/debug "document: " args)
               (into [:document] args))
   :SelectionSet (fn selection-set [& args]
                   (log/debug "SelectionSet: " args)
                   [:selection-set args])
   :Selection (fn selection [& args]
                (log/debug "Selection: " args)
                (let [props (into {} args)]
                  [:selection props]))
   :Field (fn field [& args]
            (log/debug "Field: " args)
            [:field (into {} args)])
   :Arguments (fn arguments [& args]
                (log/debug "Arguments: " args)
                [:arguments (into {} args)])
   :Argument (fn argument [& args]
               (log/debug "Argument: " args)
               (let [m (into {} args)
                     name (:name m)
                     value (:value m)]
                 [name value]))
   :IntValue (fn int-value [v]
               (log/debug "IntValue: " v)
               (Integer/parseInt v))
   :FloatValue (fn float-value [v]
                 (log/debug "FloatValue: " v)
                 (Double. v))
   :StringValue (fn string-value [& args]
                  (log/debug "StringValue: " args)
                  (clojure.string/join (map second args)))
   :Name (fn name [v]
           (log/debug "Name: " v)
           [:name v])
   :Value (fn value [v]
            (log/debug "Value: " v)
            [:value v])
   :FragmentDefinition (fn fragment-definition [& args]
                         (log/debug "FragmentDefinition: " args)
                         (let [definition (into {} args)
                               fragment-name (:fragment-name definition)]
                           [:fragment-definition definition]))
   :TypeCondition (fn type-condition [v]
                    (log/debug "TypeCondition: " v)
                    [:type-condition v])
   :NamedType (fn named-type [v]
                (log/debug "NamedType: " v)
                {:named-type (second v)})
   :FragmentName (fn fragment-name [v]
                   (log/debug "FragmentName: " v)
                   [:fragment-name (second v)])
   :Directive (fn directive [& args]
                (log/debug "Directive: " args)
                [:directive (into {} args)])
   :FragmentSpread (fn fragment-spread [& args]
                     (log/debug "FragmentSpread: " args)
                     [:fragment-spread (into {} args)])
   :InlineFragment (fn inline-fragment [& args]
                     (log/debug "InlineFragment: " args)
                     [:inline-fragment (into {} args)])})

(defn transform
  [parse-tree]
  (insta/transform
   transformation-map
   parse-tree))

(comment
  ;; Sample expressions
  (parse "query {user}")
  (parse "query {user {id}}")
  (transform (parse "query {user {id}}"))
  (transform (parse "type Person {
  name: String
  age: Int
  picture: Url
}
")))
