(defproject galapagos "0.1.0-SNAPSHOT"
  :description "An exploration of the GraphQL specification in Clojure"
  :url "https://github.com/jstaffans/galapagos"
  :license {:name  "The MIT License"
            :url  "http://opensource.org/licenses/MIT"
            :distribution :repo}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [instaparse "1.4.1"]
                 [prismatic/schema "1.0.0"]
                 [com.taoensso/timbre "4.1.1"]
                 [medley "0.7.0"]
                 [muse "0.4.0"]
                 [cats "0.4.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]]
  :profiles {:dev
             {:dependencies [[juxt/iota "0.1.2"]]
              :plugins      [[michaelblume/lein-marginalia "0.9.0"]]}})
