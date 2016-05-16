(ns studfinder.db
  (:require [studfinder.scrape :as scrape]
            [datomic.api :as d]))

(defn save-lot
  [db list-id lot]
  (print ".")
  @(d/transact db [{:db/id #db/id[:db.part/user]
                    :part/name (:name lot)
                    :part/list [list-id]}])
  @(d/transact db [{:db/id #db/id[:db.part/user]
                    :store/name (:store-name lot)
                    :store/username (:store-username lot)
                    :store/url (:store-url lot)
                    :store/rep (:store-rep lot)}])
  @(d/transact db [{:db/id #db/id[:db.part/user]
                    :lot/quantity (:quantity lot)
                    :lot/unit-price (:unit-price lot)
                    :lot/part [:part/name (:name lot)]
                    :lot/store [:store/username (:store-username lot)]}]))

(defn save-store-policies
  [db store]
  @(d/transact db [{:db/id #db/id[:db.part/user]
                    :store/username (:store-username store)
                    :store/terms (:store-terms store)
                    :store/shipping-policy (:shipping-policy store)}]))
