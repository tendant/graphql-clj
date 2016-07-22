(defproject graphql-clj "0.1.4-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [instaparse "1.4.1"]
                 [com.taoensso/timbre "4.3.1"]
                 [rhizome "0.2.5"]]
  :profiles {:dev {:dependencies [[io.forward/yaml "1.0.3"]
                                  [org.clojure/core.match "0.3.0-alpha4"]]}}
  :plugins [[s3-wagon-private "1.1.2"]]
  :repositories [["private" {:url "s3p://eng-repos/libs/releases/"
                             :creds :gpg}]])
