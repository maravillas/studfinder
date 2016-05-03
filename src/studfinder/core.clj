(ns studfinder.core
  (:require [clj-http
             [client :as http]
             [cookies :as cookies]]
            [environ.core :refer [env]]
            [hickory
             [core :as hickory]
             [select :as s]]
            [clojure.string :as str]))

(def urls
  {:login (constantly "https://www.bricklink.com/login.asp?logInTo=my.asp")
   :wanted-detail #(str "http://www.bricklink.com/wantedDetail.asp?catType=P&wantedMoreID=" %)})

(def headers
  {"User-Agent" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.86 Safari/537.36"})

(defonce cookie-store (cookies/cookie-store))

(defn parse-url
  [url]
  (println "Requesting" url)
  (-> (http/get url {:cookie-store cookie-store})
      :body
      hickory/parse
      hickory/as-hickory))

(def anchor-hrefs
  (comp
   (map :attrs)
   (map :href)))

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
  (let [first-page (parse-url ((:wanted-detail urls) wanted-list-id))
        other-page-urls (wanted-detail-page-urls first-page)
        other-pages (map (comp parse-url #(str "http://www.bricklink.com/" %)) other-page-urls)]
    (mapcat wanted-detail-lot-urls (conj other-pages first-page))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lot list

(defn parse-min-buy
  [s]
  (when s
    (let [min (str/replace s #"Loc: [^,]+, Min Buy: (~US \$)?" "")
          min (try (Double/parseDouble min) (catch Exception e nil))]
      min)))

(defn single-content
  [e]
  (-> e first :content first))

(defn extract-lot
  [lot-row]
  (let [description (first (s/select (s/and (s/tag :td) (s/nth-child 3)) lot-row))
        store       (first (s/select (s/and (s/tag :td) (s/nth-child 4)) lot-row))
        shop        (first (s/select (s/child (s/tag :font) (s/and (s/tag :a) (s/nth-child 2))) store))]
    {:name    (-> (s/select s/first-child description)
                  single-content)
     :min-buy (-> (s/select (s/child (s/tag :font) (s/tag :font)) description)
                  single-content
                  parse-min-buy)
     :shop-name (-> shop
                    :content
                    first)
     :shop-url (str "http://www.bricklink.com" (get-in shop [:attrs :href]))}))

(defn lots
  [lot-list-page]
  (let [rows (s/select (s/descendant (s/and (s/tag :table) (s/class "tb-main-content"))
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
         (map #(str "http://www.bricklink.com" %))))))

(defn lot-list-lots
  [first-page]
  (let [lot-list-page-urls (lot-list-page-urls first-page)
        lot-list-pages (map parse-url lot-list-page-urls)]
    (mapcat lots (conj lot-list-pages first-page))))

(defn wanted-list-lots
  [wanted-list-id]
  (let [lot-list-first-pages (map #(parse-url %) (take 1 (lot-list-urls wanted-list-id)))
        ]))
