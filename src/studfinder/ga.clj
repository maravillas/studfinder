(ns studfinder.ga
  (:require [clojure
             [pprint :as pprint :refer [pprint]]
             [set :as set]]
            [datomic.api :as d]))

(defn gene
  [[part [[quantity price _ part-name store-name store-url ship-cost]]]]
  {:quantity quantity
   :unit-price price
   :part-name part-name
   :store-name store-name
   :store-url store-url
   :ship-cost ship-cost})

#_(defn format-individual
    [db {:keys [genome fitness]}]
    (let [part (d/pull '[:part/name] ())
          store (d/pull '[*] (get-in ind [:genome :store]))]
      ))

(defn sample-lots
  [lots]
  (d/q '[:find ?part (sample 1 ?combined)
         :where
         [?quantity ?price ?part ?part-name ?store-name ?store-url ?ship-cost]
         [(vector ?quantity ?price ?part ?part-name ?store-name ?store-url ?ship-cost) ?combined]]
       lots))

(defn make-individual
  [db wanted-list-id]
  (let [lots (sample-lots
              (d/q '[:find ?quantity ?price ?part ?part-name ?store-name ?store-url ?ship-cost
                     :in $ ?list-id
                     :where
                     [?list :list/id ?list-id]
                     [?list :list/part ?part]
                     [?part :part/name ?part-name]
                     [?lot :lot/quantity ?quantity]
                     [?lot :lot/unit-price ?price]
                     [?lot :lot/part ?part]
                     [?lot :lot/store ?store]
                     [?store :store/name ?store-name]
                     [?store :store/url ?store-url]
                     [(get-else $ ?store :store/ship-cost 0) ?ship-cost]]
                   (d/db db)
                   wanted-list-id))]
    {:genome (into {} (map (fn [l] [(first l) (gene l)]) lots))}))

(defn ensure-key-count
  [all-keys sampled-keys]
  (cond (empty? sampled-keys)
        #{(first all-keys)}
        
        (= (count sampled-keys) (count all-keys))
        (disj sampled-keys (first sampled-keys))

        :else
        sampled-keys))

(defn crossover
  [ind1 ind2]
  {:pre [(= (set (keys (:genome ind1)))
            (set (keys (:genome ind2))))]
   :post [(= (set (keys (:genome ind1)))
             (set (keys (:genome ind2)))
             (set (keys (:genome (first %))))
             (set (keys (:genome (second %)))))]}
  (let [genome1 (:genome ind1)
        genome2 (:genome ind2)
        keys1 (set (random-sample 0.5 (keys genome1)))
        keys1 (ensure-key-count (keys genome1) keys1)
        keys2 (set/difference (set (keys genome1)) keys1)
        genome1-genes (select-keys genome1 keys1)
        genome2-genes (select-keys genome2 keys2)]
    [{:genome (merge genome2 genome1-genes)}
     {:genome (merge genome1 genome2-genes)}]))

(defn random-gene
  [db part]
  (gene
   (first
    (sample-lots
     (d/q '[:find ?quantity ?price ?part ?part-name ?store-name ?store-url ?ship-cost
            :in $ ?part
            :where
            [?lot :lot/quantity ?quantity]
            [?lot :lot/unit-price ?price]
            [?lot :lot/part ?part]
            [?lot :lot/store ?store]
            [?part :part/name ?part-name]
            [?store :store/name ?store-name]
            [?store :store/url ?store-url]
            [(get-else $ ?store :store/ship-cost 0) ?ship-cost]]
          (d/db db)
          part)))))

(defn mutate
  [db {:keys [genome]} rate]
  {:post [(= (set (keys (:genome %))) (set (keys genome)))]}
  (let [ks (random-sample rate (keys genome))]
    {:genome (merge genome (into {} (map (fn [k] [k (random-gene db k)]) ks)))}))

(defn fitness
  [{:keys [genome]}]
  (apply +
         (map #(+ (* (:unit-price %) (:quantity %)) (:ship-cost %))
              (vals genome))))

(defn evaluate-fitness
  [inds]
  (let [fitnesses (map fitness inds)
        sum (apply + fitnesses)
        normalized (map #(/ % sum) fitnesses)]
    (map (fn [ind norm fit] (assoc ind :fitness norm :abs-fitness fit)) inds normalized fitnesses)))

(defn tournament-select
  [inds tourney-size]
  (or (->> inds
           (random-sample (/ tourney-size (count inds)))
           (sort-by :fitness)
           first)
      (first inds)))

(defn make-children
  [db inds mutation-rate]
  (let [p1 (tournament-select inds 2)
        p2 (tournament-select inds 2)
        [c1 c2] (crossover p1 p2)
        c1 (mutate db c1 mutation-rate)
        c2 (mutate db c2 mutation-rate)]
    [c1 c2]))

(defn evolve
  [db list-id pop-size mutation-rate generations]
  (loop [gen 0
         inds (repeatedly pop-size #(make-individual db list-id))]
    (println "Generation" gen)
    (let [with-fitness (evaluate-fitness inds)
          _ (println with-fitness)
          next-inds (mapcat (fn [_] (make-children db with-fitness mutation-rate)) (range (/ pop-size 2)))]
      (if (< (inc gen) generations)
        (recur (inc gen) next-inds)
        (first (sort-by :fitness (evaluate-fitness next-inds)))))))
