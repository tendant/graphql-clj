(defproject graphql-clj "0.1.5-SNAPSHOT"
  :description "A Clojure library designed to provide GraphQL implementation."
  :url "https://github.com/tendant/graphql-clj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [instaparse "1.4.1"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [com.taoensso/timbre "4.3.1"]]
  :profiles {:dev {:dependencies [[io.forward/yaml "1.0.3"]]}})
