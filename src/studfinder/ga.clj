(ns studfinder.ga
  (:require [datomic.api :as d]))

(defn gene
  [[part [[quantity price _ store ship-cost]]]]
  {:quantity quantity
   :unit-price price
   :part part
   :store store
   :ship-cost ship-cost})

(defn sample-lots
  [lots]
  (d/q '[:find ?part (sample 1 ?combined)
         :where
         [?quantity ?price ?part ?store ?ship-cost]
         [(vector ?quantity ?price ?part ?store ?ship-cost) ?combined]]
       lots))

(defn make-individual
  [db wanted-list-id]
  (let [lots (sample-lots
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
    (into {} (map (fn [l] [(first l) (gene l)]) lots))))

(defn recombine
  [ind1 ind2]
  (let [ind1-keys (random-sample 0.5 (keys ind1))
        ind1-genes (select-keys ind1 ind1-keys)]
    (merge ind2 ind1-genes)))

(defn random-gene
  [db part]
  (gene
   (sample-lots
    (d/q '[:find ?quantity ?price ?part ?store ?ship-cost
           :in $ ?part
           :where
           [?lot :lot/quantity ?quantity]
           [?lot :lot/unit-price ?price]
           [?lot :lot/part ?part]
           [?lot :lot/store ?store]
           [(get-else $ ?store :store/ship-cost 0) ?ship-cost]]
         (d/db db)
         part))))

(defn mutate
  [db ind rate]
  (let [ks (random-sample rate (keys ind))
        new-genes (map (fn [k] [k (d/q '[:find ?])]) ks)]
    (merge ind (into {} (map (fn [k] [k (random-gene db k)]) ks)))))

(defn fitness
  [ind]
  (apply +
         (map #(+ (* (:unit-price %) (:quantity %)) (:ship-cost %))
              (vals ind))))

(defn relative-fitnesses
  [inds]
  (let [fitnesses (map fitness inds)
        max (apply max fitnesses)]
    (map #(- max %) fitnesses)))

(defn select
  [inds])
