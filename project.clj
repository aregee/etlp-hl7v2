(defproject org.clojars.aregee/etlp-hl7v2 "0.1.0-SNAPSHOT"
  :description "Extensible bottomup hl7v2 parser with transducer interface"
  :url "http://github.com/aregee/etlp-hl7v2"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [clj-commons/clj-yaml "0.7.0"]
                 [cheshire "5.11.0"]
                 [healthsamurai/matcho "0.3.9"]]
  :deploy-repositories {"releases" {:url "https://repo.clojars.org" :creds :auth}}
  :repl-options {:init-ns etlp-hl7v2.core}
  :profiles {:test {:dependencies [[healthsamurai/matcho "0.3.9"]]}})
