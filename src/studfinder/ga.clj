(ns studfinder.ga
  (:require [clojure
             [pprint :as pprint :refer [pprint]]
             [set :as set]]
            [datomic.api :as d])
  (:import [java.util.Date]))

(defn format-cents
  [d]
  (format "$%.2f" (/ d 100.0)))

(defn gene
  [[part quantity price part-name
    store store-name store-url ship-cost :as g]]
  {:quantity quantity
   :unit-price price
   :part-name part-name
   :store store
   :store-name store-name
   :store-url store-url
   :ship-cost ship-cost})

(defn format-part
  [{:keys [part-name quantity unit-price]}]
  (str "  " part-name ": "
       quantity " @ " (format-cents unit-price) " = "
       (format-cents (* quantity unit-price)) "\n"))

(defn cost
  [{:keys [genome]}]
  (let [part-cost (->> (vals genome)
                       (map #(* (:unit-price %) (:quantity %)))
                       (apply +))
        shipping-cost (->> (vals genome)
                           (map #(select-keys % [:ship-cost :store-name]))
                           (into #{})
                           (map :ship-cost)
                           (apply +))]
    (+ part-cost shipping-cost)))

(defn format-individual
  [{:keys [genome] :as ind}]
  (let [stores (group-by (juxt :store-name :store-url :ship-cost) (vals genome))]
    (apply str
           (count stores) " stores totalling " (format-cents (cost ind)) ":\n"
           (map (fn [[[store-name url] parts]]
                  (str store-name " (" url "):\n"
                       (apply str (map format-part parts))
                       "\n"))
                stores))))

(defn sample-lots
  [lots]
  (d/q '[:find ?part (sample 1 ?combined)
         :where
         [?part ?quantity ?price ?part-name
          ?store ?store-name ?store-url ?ship-cost]
         [(vector ?part ?quantity ?price ?part-name
                  ?store ?store-name ?store-url ?ship-cost) ?combined]]
       lots))

(defn lots-for-list
  ([db wanted-list-id]
   (lots-for-list db wanted-list-id nil))
  ([db wanted-list-id stores]
   (let [find  '[:find
                 ?part ?quantity ?price ?part-name
                 ?store ?store-name ?store-url ?ship-cost]
         in    (if stores
                 '[:in $ ?list-id [?stores ...]]
                 '[:in $ ?list-id])
         where '[:where
                 [?list :list/id ?list-id]
                 [?list :list/part ?part]
                 [?part :part/name ?part-name]
                 [?lot :lot/part ?part]
                 [?lot :lot/store ?store]
                 [?store :store/rep ?rep]
                 [(> ?rep 500)]
                 [?lot :lot/quantity ?quantity]
                 [?lot :lot/unit-price ?price]
                 [?store :store/name ?store-name]
                 [?store :store/url ?store-url]
                 [(get-else $ ?store :store/ship-cost 0) ?ship-cost]]]
     (if stores
       (d/q (concat find
                    in
                    where)
            (d/db db)
            wanted-list-id stores)
       (d/q (concat find
                    in
                    where)
            (d/db db)
            wanted-list-id)))))

(defn group-lots-by-part
  [lots]
  (group-by first lots))

(defn lots-for-individual
  [db list-id {:keys [genome]}]
  (lots-for-list db list-id (->> (vals genome)
                                 (map :store)
                                 (into #{}))))

(defn make-individuals
  [db lots n]
  (repeatedly n (fn [] {:genome (->> lots
                                     sample-lots
                                     (map (fn [[part vs]] [part (gene (first vs))]))
                                     (into {}))})))

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
        pivot (rand-int (count genome1))
        [keys1 keys2] (split-at pivot (sort (keys genome1)))
        genome1-genes (select-keys genome1 keys1)
        genome2-genes (select-keys genome2 keys2)]
    [{:genome (merge genome2 genome1-genes)}
     {:genome (merge genome1 genome2-genes)}]))

(defn random-gene
  [lots]
  (gene (rand-nth lots)))

(defn mutate
  [lots {:keys [genome] :as ind} rate]
  {:pre [(= (set (keys lots)) (set (keys genome)))]
   :post [(= (set (keys (:genome %))) (set (keys genome)))]}
  (let [ks (random-sample rate (keys genome))
        new-genes (mapcat (fn [k] [k (random-gene (get lots k))]) ks)]
    (if (empty? new-genes)
      ind
      (update-in ind [:genome] #(apply assoc % new-genes)))))

(defn fitness
  [{:keys [genome]}]
  (let [part-cost (->> (vals genome)
                       (map #(* (:unit-price %) (:quantity %)))
                       (apply +))
        stores  (->> (vals genome)
                     (map #(select-keys % [:ship-cost :store-name]))
                     (into #{}))
        shipping-cost (->> stores
                           (map :ship-cost)
                           (apply +))]
    (* (+ part-cost shipping-cost)
       (/ (count stores) 2))))

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
  [lots inds mutation-rate]
  (let [p1 (tournament-select inds 2)
        p2 (tournament-select inds 2)
        [c1 c2] (crossover p1 p2)
        c1 (mutate lots c1 mutation-rate)
        c2 (mutate lots c2 mutation-rate)]
    [c1 c2]))

(defn evolve
  ([db list-id pop-size mutation-rate generations]
   (evolve db list-id pop-size mutation-rate generations nil))
  ([db list-id pop-size mutation-rate generations archetype-individual]
   (let [all-lots (if archetype-individual
                    (lots-for-individual db list-id archetype-individual)
                    (lots-for-list db list-id))
         lots-by-part (group-lots-by-part all-lots)]
     (loop [gen 0
            inds (make-individuals db all-lots pop-size)
            time (.getTime (java.util.Date.))
            best nil]
       (print (str "Generation " gen ": "))
       (let [with-fitness (evaluate-fitness inds)
             winnar (first (sort-by :fitness with-fitness))
             next-inds (mapcat (fn [_] (make-children lots-by-part with-fitness mutation-rate)) (range (/ pop-size 2)))]
         (if (< (inc gen) generations)
           (do
             (println (format "%.2f" (/ (- (.getTime (java.util.Date.)) time) 1000.0))
                      "seconds, local best"
                      (:abs-fitness winnar)
                      "global best"
                      (:abs-fitness best))
             (recur (inc gen)
                    next-inds
                    (.getTime (java.util.Date.))
                    (if (or (nil? best)
                            (< (:abs-fitness winnar) (:abs-fitness best)))
                      winnar
                      best)))
           (let [winnar (first (sort-by :fitness (evaluate-fitness next-inds)))]
             (print (str "\n----------\n"
                         (format-individual best)
                         "----------\n"))
             winnar)))))))
