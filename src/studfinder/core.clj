(ns studfinder.core
  (:require [clj-http
             [client :as http]
             [cookies :as cookies]]
            [clojure.string :as str]
            [environ.core :refer [env]]
            [hickory
             [convert :as convert]
             [core :as hickory]
             [select :as s]
             [zip :as hzip]])
  (:import java.util.Date))

(def urls
  {:login          (constantly "https://www.bricklink.com/login.asp?logInTo=my.asp")
   :wanted-detail  #(str "http://www.bricklink.com/wantedDetail.asp?catType=P&wantedMoreID=" %)
   :store-page     #(str "https://www.bricklink.com/store.asp?p=" %)})

(def headers
  {"User-Agent" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.86 Safari/537.36"})

(def min-request-delay 1000)

(defonce cookie-store (cookies/cookie-store))

(defonce last-request (atom (.getTime (Date.))))

(defn delay-fetch
  []
  (let [to-wait (max 0 (- min-request-delay (- (.getTime (Date.)) @last-request)))]
    (when-not (zero? to-wait)
      (println "Waiting" to-wait "ms")
      (Thread/sleep to-wait))))

(defn fetch
  [url]
  (delay-fetch)
  (reset! last-request (.getTime (Date.)))
  (println "Requesting" url)
  (-> (http/get url {:cookie-store cookie-store})
      :body
      hickory/parse
      hickory/as-hickory))

(def anchor-hrefs
  (comp
   (map :attrs)
   (map :href)))

(defn prefix-url
  [s]
  (str "http://www.bricklink.com" (if (.startsWith s "/") s (str "/" s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Login

(defn login
  [username password]
  (http/post ((:login urls)) {:debug true :debug-body true
                              :headers headers
                              :form-params {:frmUsername username
                                            :frmPassword password
                                            :a "a"}
                              :cookie-store cookie-store}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wanted list detail

(defn wanted-detail-lot-urls
  "Lists all \"view all lots\" URLs on a single wanted list detail page."
  [page]
  (sequence (comp anchor-hrefs
                  (map #(str "http://www.bricklink.com" % "&sz=500&excBind=Y")))
            (s/select (s/descendant (s/and (s/tag :table) (s/class "ta"))
                                    (s/and (s/tag :font) (s/class "fv"))
                                    (s/tag :a))
                      page)))

(defn wanted-detail-page-urls
  "Lists URLs for all of a wanted list's detail pages."
  [first-page]
  (->> (s/select (s/descendant (s/child (s/and (s/tag :table) (s/class "tb-main-content"))
                                        (s/tag :tbody)
                                        (s/and (s/tag :tr) s/first-child)
                                        (s/tag :td)
                                        (s/tag :table))
                               (s/and (s/tag :p) (s/nth-child 5))
                               (s/tag :a))
                 first-page)
       (sequence
        (comp
         (remove #(#{["Next"] ["Previous"]} (:content %)))
         anchor-hrefs))))

(defn lot-list-urls
  "Lists all \"view all lots\" URLs on all of a wanted list's detail pages."
  [wanted-list-id]
  (println "--- Collecting lots-for-parts URLs for wanted list ---")
  (let [first-page (fetch ((:wanted-detail urls) wanted-list-id))
        other-page-urls (wanted-detail-page-urls first-page)
        other-pages (map (comp fetch prefix-url) other-page-urls)]
    (mapcat wanted-detail-lot-urls (conj other-pages first-page))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lot list

(defn parse-price
  [s]
  (try
    (if (= s "None")
      0
      (let [[dollaz cents] (str/split s #"\.")
            cents (apply str (take 2 cents))]
        (+ (* (Integer/parseInt dollaz) 100)
           (Integer/parseInt cents))))
    (catch Exception e
      (throw (ex-info "Failed to parse price" {:s s :cause e})))))

(defn parse-min-buy
  [s]
  (try
    (when s
      (-> s
          (str/replace #"Loc: [^,]+, Min Buy: (~US \$)?" "")
          parse-price))
    (catch Exception e
      (if (instance? clojure.lang.ExceptionInfo e)
        (throw e)
        (throw (ex-info "Failed to parse minimum buy amount" {:s s :cause e}))))))

(defn parse-rep
  [s]
  (try
    (-> s
        (str/replace #"[()]" "")
        Integer/parseInt)
    (catch Exception e
      (throw (ex-info "Failed to parse reputation" {:s s :cause e})))))

(defn parse-quantity
  [s]
  (try
    (-> s
        (str/replace #"," "")
        Integer/parseInt)
    (catch Exception e
      (throw (ex-info "Failed to parse quantity" {:s s :cause e})))))

(defn parse-unit-price
  [s]
  (try
    (when s
      (-> s
          (str/replace #"US \$" "")
          (str/replace #"[()]" "")
          parse-price))
    (catch Exception e
      (if (instance? clojure.lang.ExceptionInfo e)
        (throw e)
        (throw (ex-info "Failed to parse unit-price" {:s s :cause e}))))))

(defn parse-shop-username
  [s]
  (second (re-find #"p=([^&]+)" s)))

(defn single-content
  [e]
  (-> e first :content first))

(defn find-by-label
  [l label]
  (cond (re-find label (first l)) (first (rest l))
        (empty? l) nil
        :else (recur (rest l) label)))

(defn extract-lot
  [lot-row]
  (let [description  (first (s/select (s/child (s/and (s/tag :td) (s/nth-child 3))) lot-row))
        available-in (first (s/select (s/child (s/and (s/tag :td) (s/nth-child 4)) (s/tag :font)) lot-row))
        shop         (s/select (s/and (s/tag :a) (s/nth-child 2)) available-in)
        rep          (s/select (s/and (s/tag :a) (s/nth-child 3)) available-in)
        texts        (s/select (s/not (s/node-type :element)) available-in)
        qty          (find-by-label texts #"Qty")
        price        (find-by-label texts #"Each")]
    {:name          (-> (s/select s/first-child description)
                        single-content)
     :min-buy       (-> (s/select (s/child (s/tag :font) (s/tag :font)) description)
                        single-content
                        parse-min-buy)
     :shop-username (-> shop
                        first
                        :attrs
                        :href
                        parse-shop-username)
     :shop-name     (-> shop
                        single-content)
     :shop-url      (prefix-url (get-in (first shop) [:attrs :href]))
     :shop-rep      (-> rep
                        single-content
                        parse-rep)
     :quantity      (-> qty
                        parse-quantity)
     :unit-price    (-> price
                        parse-unit-price)}))

(defn lots
  [lot-list-page]
  (let [rows (s/select (s/descendant (s/and (s/tag :table) (s/class "tb-main-content"))
                                     (s/tag :table)
                                     (s/tag :table)
                                     (s/and (s/tag :tr) (s/class "tm")))
                       lot-list-page)]
    (map extract-lot rows)))

(defn lot-list-page-urls
  [first-page]
  (->> (s/select (s/descendant (s/child (s/and (s/tag :table) (s/class "tb-main-content"))
                                        (s/tag :tbody)
                                        (s/and (s/tag :tr) s/first-child)
                                        (s/tag :td)
                                        (s/tag :table))
                               (s/and (s/tag :font) (s/nth-child 5))
                               (s/tag :a))
                 first-page)
       (sequence
        (comp
         (remove #(#{["Next"] ["Previous"]} (:content %)))
         anchor-hrefs
         (map prefix-url)))))

(defn lot-list-lots
  [first-page]
  (println "--- Collecting lots for a part ---")
  (let [lot-list-page-urls (lot-list-page-urls first-page)
        lot-list-pages (map fetch lot-list-page-urls)]
    (mapcat lots (conj lot-list-pages first-page))))

(defn wanted-list-lots
  [wanted-list-id]
  (let [lot-list-first-pages (map #(fetch %) (take 2 (lot-list-urls wanted-list-id)))
        lots (map lot-list-lots lot-list-first-pages)]
    lots))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Store terms

(defn gross-nested-table-successor-selector
  [text-re]
  ;; follow-adjacent selects the tr that follows the tr containing text-re
  (s/follow-adjacent (s/and (s/child (s/tag :body) (s/tag :center) (s/tag :table) (s/tag :tbody) (s/tag :tr))
                            (s/has-descendant (s/find-in-text text-re)))
                     (s/tag :tr)))

(defn terms-text
  [terms-page]
  (-> (s/select (s/descendant (gross-nested-table-successor-selector #"Store Terms & Conditions")
                              (s/tag :font)) terms-page)
      single-content))

(defn shipping-text
  [terms-page]
  (-> (s/select (s/descendant (gross-nested-table-successor-selector #"Store Shipping Policy")
                              (s/tag :font)) terms-page)
      single-content))

(defn store-terms
  [store-username]
  (let [framed-page (fetch ((:store-page urls) store-username))
        top-url     (-> (s/select (s/tag :frame) framed-page) first :attrs :src)
        top-page    (fetch (prefix-url top-url))
        terms-url   (-> (s/select (s/and (s/tag :a)
                                         (s/has-descendant (s/find-in-text #"Store Terms")))
                                  top-page)
                        first
                        :attrs
                        :href)
        terms-page  (fetch (prefix-url terms-url))]
    {:store-terms     (terms-text terms-page)
     :shipping-policy (shipping-text terms-page)}))
