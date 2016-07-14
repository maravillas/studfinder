(ns studfinder.ga
  (:require [clojure
             [pprint :as pprint :refer [pprint]]
             [set :as set]]
            [datomic.api :as d]
            [incanter
             [charts :as charts]
             [core :as incanter]]))

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
  (pmap (fn [_] {:genome (->> lots
                              sample-lots
                              (map (fn [[part vs]] [part (gene (first vs))]))
                              (into {}))})
        (range n)))

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

(defn log-store-weight
  [c]
  (Math/log10 c))

(defn half-store-weight
  [c]
  (/ c 2))

(defn fourth-store-weight
  [c]
  (/ c 4))

(defn fitness
  [store-weight-fn {:keys [genome]}]
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
       (store-weight-fn (count stores)))))

(defn evaluate-fitness
  [inds store-weight-fn]
  (let [fitnesses (map #(fitness store-weight-fn %) inds)
        sum (apply + fitnesses)
        normalized (map #(/ % sum) fitnesses)]
    (pmap (fn [ind norm fit] (assoc ind :fitness norm :abs-fitness fit)) inds normalized fitnesses)))

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

(defn make-generation
  [lots evaluated-inds mutation-rate]
  (apply concat (pmap (fn [_] (make-children lots evaluated-inds mutation-rate))
                      (range (/ (count evaluated-inds) 2)))))

(defn summarize-gen
  [gen start-time winnar best]
  (println (format "Generation %d: %.2fs, local\t%7d\t, global %7d (%d, %s)"
                   gen
                   (/ (- (.getTime (java.util.Date.)) start-time) 1000.0)
                   (int (:abs-fitness winnar))
                   (when (:abs-fitness best) (int (:abs-fitness best)))
                   (count (set (map :store (vals (:genome best)))))
                   (format-cents (cost best)))))

(defn evolve
  ([db list-id pop-size mutation-rate generations]
   (evolve db list-id pop-size mutation-rate generations #(/ % 2)))
  ([db list-id pop-size mutation-rate generations store-weight-fn]
   (evolve db list-id pop-size mutation-rate generations store-weight-fn nil))
  ([db list-id pop-size mutation-rate generations store-weight-fn archetype-individual]
   (let [all-lots (if archetype-individual
                    (lots-for-individual db list-id archetype-individual)
                    (lots-for-list db list-id))
         lots-by-part (group-lots-by-part all-lots)]
     (loop [gen 0
            log []
            inds (make-individuals db all-lots pop-size)
            time (.getTime (java.util.Date.))
            best nil]
       (let [evaluated-inds (evaluate-fitness inds store-weight-fn)
             winnar (first (sort-by :fitness evaluated-inds))
             next-inds (make-generation lots-by-part evaluated-inds mutation-rate)]
         (if (<= (inc gen) generations)
           (do
             (summarize-gen gen time winnar best)
             (recur (inc gen)
                    (conj log {:abs-fitness (:abs-fitness winnar)
                               :cost (/ (cost winnar) 100.0)
                               :stores (count (set (map :store (vals (:genome winnar)))))})
                    next-inds
                    (.getTime (java.util.Date.))
                    (if (or (nil? best)
                            (< (:abs-fitness winnar) (:abs-fitness best)))
                      winnar
                      best)))
           (let [winnar (first (sort-by :fitness (evaluate-fitness next-inds store-weight-fn)))]
             (print (str "\n----------\n"
                         (format-individual best)
                         "----------\n"))
             [winnar log])))))))

(defn results-to-dataset
  [results]
  (incanter/to-dataset
   (mapcat (fn [[k [winner log]]]
             (map-indexed (fn [i g] (assoc g
                                           :generation i
                                           :experiment k)) log))
           results)))

(defn run-experiments
  [db list-id generations settings]
  (let [results (map (fn [{:keys [pop-size mutation-rate store-weight-fn name] :as s}]
                       (println "Running experiment " s)
                       [name (evolve db list-id pop-size mutation-rate generations store-weight-fn)])
                     settings)]
    results))

(defn chart-experiments
  [dataset]
  (incanter/with-data dataset
   (incanter/view (charts/line-chart :generation :cost
                                     :group-by :experiment
                                     :legend true))))

