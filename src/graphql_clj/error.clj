(ns graphql-clj.error)

(defn throw-error
  ([^String msg data]
   (throw (ex-info msg (assoc data :errors [msg]))))
  ([^String msg]
   (throw-error msg {})))
