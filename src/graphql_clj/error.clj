(ns graphql-clj.error)

(defn throw-error
  ([^String msg data]
   (println "throw error: " msg)
   (throw (ex-info msg (assoc data :message msg))))
  ([^String msg]
   (throw-error msg {})))
