(defproject rhombrick-survey "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [rhombrick "1.0.0-SNAPSHOT"]
                ]
  :main rhombrick-survey.core
  ;:warn-on-reflection true
  :uberjar [:aot :all]
  :jvm-opts ["-Xmx1g"]
)
