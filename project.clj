(defproject galapagos "0.1.0-SNAPSHOT"
  :description "An exploration of the GraphQL specification in Clojure"
  :url "https://github.com/jstaffans/galapagos"
  :license {:name "WTFPL"
            :url "http://www.wtfpl.net"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [instaparse "1.4.1"]
                 [prismatic/plumbing "0.4.4"]
                 [prismatic/schema "0.4.3"]]
  :profiles {:dev {:dependencies [[juxt/iota "0.1.2"]]}})
