(ns graphql-clj.error)

(defn throw-error
  ([^String msg data]
   (throw (ex-info msg (assoc data :message msg))))
  ([^String msg]
   (throw-error msg {})))
