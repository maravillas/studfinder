(ns studfinder.ga
  (:require [datomic.api :as d]))

(defn random-individual
  [db wanted-list-id]
  (let [lots (d/q '[:find ?part (sample 1 ?combined)
                    :where
                    [?quantity ?price ?part ?store ?ship-cost]
                    [(vector ?quantity ?price ?part ?store ?ship-cost) ?combined]]
                  (d/q '[:find ?quantity ?price ?part ?store ?ship-cost
                         :in $ ?list-id
                         :where
                         [?list :list/id ?list-id]
                         [?list :list/part ?part]
                         [?lot :lot/quantity ?quantity]
                         [?lot :lot/unit-price ?price]
                         [?lot :lot/part ?part]
                         [?lot :lot/store ?store]
                         [(get-else $ ?store :store/ship-cost 0) ?ship-cost]]
                       (d/db db)
                       wanted-list-id))]
    (into {}
          (map (fn [[part [[quantity price _ store ship-cost]]]]
                 [part {:quantity quantity
                        :unit-price price
                        :part part
                        :store store
                        :ship-cost ship-cost}])
               lots))))
