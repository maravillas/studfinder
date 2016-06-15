(defproject studfinder "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [hickory "0.6.0"]
                 [environ "1.0.2"]
                 [clj-http "3.0.1"]
                 [com.datomic/datomic-free "0.9.5359"]
                 [incanter "1.5.7"]]
  :plugins [[lein-environ "1.0.2"]
            [lein-datomic "0.2.0"]]
  :datomic {:config "resources/free-transactor-template.properties"
            :db-uri "datomic:free://localhost:4334/studfinder"
            :schemas ["resources/schema" ["schema.edn"
                                          "initial-data.edn"]]})
