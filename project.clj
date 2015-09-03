(defproject galapagos "0.1.0-SNAPSHOT"
  :description "An exploration of the GraphQL specification in Clojure"
  :url "https://github.com/jstaffans/galapagos"
  :license {:name "WTFPL"
            :url "http://www.wtfpl.net"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [instaparse "1.4.1"]
                 [prismatic/schema "1.0.0"]
                 [muse "0.4.0"]
                 [cats "0.4.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]]
  :profiles {:dev {:dependencies [[juxt/iota "0.1.2"]]}})
