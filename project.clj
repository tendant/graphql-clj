(defproject graphql-clj "0.2.4"
  :description "A Clojure library that provides a GraphQL implementation."
  :url "https://github.com/tendant/graphql-clj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :java-source-paths ["java"]
  :javac-options     ["-target" "1.8" "-source" "1.8"]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [instaparse "1.4.3"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [camel-snake-kebab "0.4.0"]]
  :profiles {:dev {:dependencies [[io.forward/yaml "1.0.3"]]}})
