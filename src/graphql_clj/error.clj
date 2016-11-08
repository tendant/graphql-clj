(ns graphql-clj.error)

(defn throw-error
  ([^String msg data]
   (throw (ex-info msg (assoc data :error msg))))
  ([^String msg]
   (throw-error msg {})))
