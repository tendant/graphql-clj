(ns graphql-clj.type
  (:require [taoensso.timbre :as log]))

(def type-map
  {:GraphQLString {:name "String"
                   :kind :SCALAR}
   :UserType {:name "User"
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
                                  [{:id "friend1"}
                                   {:id "friend2"}])}
   :FriendType {:name "Friend"
                :kind :OBJECT
                :fields {:id {:type :GraphQLString}}
                :resolve-fn (fn [& args]
                              {:id "friend1"})}
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
                                     :url "http://test.url.com"}))}})

(def schema
  {:query {:name "Query"
           :kind :OBJECT
           :fields {:user {:type :UserType}}
           :resolve-fn (fn [& args]
                         (let [parent (first args)]
                           (log/debug "query resolve-fn:" parent)
                           (identity parent)))}})

(defn get-type-meta
  [type-key]
  (or (get schema type-key)
      (get type-map type-key)
      (throw (ex-info (format "Unknown object type: %s." type-key)
                      {:type-key type-key}))))
