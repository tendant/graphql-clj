(ns graphql-clj.type
  (:require [taoensso.timbre :as log]))

(def demo-schema
  {:UserType {:name "User"
              :kind :OBJECT
              :fields {:id {:type :GraphQLString}
                       :name {:type :GraphQLString}
                       :profilePic {:type :ProfilePicType}
                       :friends {:type :FriendListType}}
              :args {}
              :resolve-fn (fn [& args]
                            (let [parent (first args)]
                              (log/debug "user resolve-fn: ")
                              (log/debug "user resolve-fn: parent: " parent)
                              {:id "test"
                               :name "good"
                               :additional "extra"}))}
   :FriendListType {:name "FriendListType"
                    :kind :LIST
                    :innerType :FriendType
                    :resolve-fn (fn [& args]
                                  [{:id "1"
                                    :name "friend 1"}
                                   {:id "2"
                                    :name "friend 2"}])}
   :FriendType {:name "Friend"
                :kind :OBJECT
                :fields {:id {:type :GraphQLString}
                         :name {:type :GraphQLString}}
                :resolve-fn (fn [& args]
                              (let [parent (first args)
                                    arguments (second args)]
                                {:id "friend1"}))}
   :ProfilePicType {:name "ProfilePic"
                    :kind :OBJECT
                    :fields {:resolution {:type :GraphQLString}
                             :url {:type :GraphQLString}}
                    :args {}
                    :resolve-fn (fn [& args]
                                  (let [parent (first args)
                                        arguments (second args)]
                                    (log/debug "profile pic resolve-fn.")
                                    (log/debug "profile pic parent: " parent)
                                    (log/debug "profile pic arguments: " arguments)
                                    {:resolution "480"
                                     :url "http://test.url.com"}))}
   :query {:name "Query"
           :kind :OBJECT
           :fields {:user {:type :UserType}}
           :resolve-fn (fn [& args]
                         (let [parent (first args)]
                           (log/debug "query resolve-fn:" parent)
                           (identity parent)))}})

(def ^{:private true} system-schema
  {:GraphQLString {:name "String"
                    :kind :SCALAR}
    :TypeType {:name "Type"
               :kind :OBJECT
               :fields {:name {:type :GraphQLString}}}
    :TypeListType {:name "TypeList"
                   :kind :LIST
                   :innerType :TypeType
                   :resolve-fn (fn [& args]
                                 (println "TOBEUPDATED"))}
    :SchemaType {:name "Schema"
                 :kind :OBJECT
                 :fields {:types {:type :TypeListType}}
                 :args {}
                 :resolve-fn identity}})

(defn create-type-meta-fn [schema]
  (let [updated-schema (update-in schema [:query :fields]
                                  assoc :__schema {:type :SchemaType})
        type-list-resolve-fn (fn [& args]
                               (vals (merge system-schema updated-schema)))
        updated-system-schema (update-in system-schema [:TypeListType]
                                         assoc :resolve-fn type-list-resolve-fn)
        merged-schema (merge updated-system-schema updated-schema)]
    (fn [type-key]
      (or (get merged-schema type-key)
          (throw (ex-info (format "Unknown object type: %s." type-key)
                          {:type-key type-key}))))))
