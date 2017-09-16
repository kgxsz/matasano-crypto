(defproject matasano-crypto "0.1.0-SNAPSHOT"
  :description "Matasano Crypto Challenge"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot matasano-crypto.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
