(ns studfinder.core
  (:require [studfinder.db :as db]
            [studfinder.scrape :as scrape]
            [datomic.api :as d]
            [environ.core :refer [env]]
            [studfinder.ga :as ga]))

(def uri "datomic:free://localhost:4334/studfinder")

(defn reset-db
  []
  (d/delete-database uri)
  (d/create-database uri)
  (let [conn (d/connect uri)
        schema-tx (read-string (slurp "resources/schema.edn"))]
    (d/transact conn schema-tx)))

(defn scrape-list
  [wanted-list-id]
  (scrape/login (env :username) (env :password))
  (let [conn (d/connect uri)
        lot-groups (scrape/wanted-list-lots wanted-list-id)]
    (doseq [group lot-groups]
      (println (str "Saving lots for part \"" (:name (first group)) "\""))
      (doseq [lot group]
        (db/save-lot conn wanted-list-id lot))
      (println ""))))
