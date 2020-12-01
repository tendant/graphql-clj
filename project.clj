(defproject graphql-clj "0.3.0-alpha-7"
  :description "A Clojure library that provides a GraphQL implementation."
  :url "https://github.com/tendant/graphql-clj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :java-source-paths ["java"]
  :javac-options     ["-target" "1.8" "-source" "1.8"]
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [instaparse "1.4.10"]
                 [org.clojure/core.match "1.0.0"]
                 [camel-snake-kebab "0.4.0"]
                 [org.antlr/antlr4-runtime "4.8-1"]
                 [clj-antlr "0.2.6"]
                 [com.walmartlabs/lacinia "0.38.0-alpha-7"]]
  :profiles {:dev {:dependencies [[io.forward/yaml "1.0.10"]
                                  [org.antlr/antlr4 "4.8-1"]]}}
  :plugins [[lein-shell "0.3.0"]]
  )
