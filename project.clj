(defproject stackfn-sandbox "0.1.0-SNAPSHOT"
  :description "Interpreter for stack-based language"
  :dependencies [[org.clojure/clojure "1.10.3"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}
             :test {:dependencies [[com.github.seancorfield/expectations "2.0.160"]]}})
