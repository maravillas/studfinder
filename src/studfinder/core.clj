(ns studfinder.core
  (:require [clj-http
             [client :as http]
             [cookies :as cookies]]
            [environ.core :refer [env]]
            [hickory
             [core :as hickory]
             [select :as s]]))

(def urls
  {:login (constantly "https://www.bricklink.com/login.asp?logInTo=my.asp")
   :wanted #(str "http://www.bricklink.com/wantedDetail.asp?catType=P&wantedMoreID=" %)})

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

(defn login
  [username password]
  (http/post ((:login urls)) {:debug true :debug-body true
                              :headers headers
                              :form-params {:frmUsername username
                                            :frmPassword password
                                            :a "a"}
                              :cookie-store cookie-store}))

(defn part-urls-on-wanted-page
  [page]
  (sequence (comp anchor-hrefs (map #(str % "&sz=500&excBind=Y")))
            (s/select (s/descendant (s/and (s/tag :table) (s/class "ta"))
                                    (s/and (s/tag :font) (s/class "fv"))
                                    (s/tag :a))
                      page)))

(defn wanted-page-urls-on-wanted-page
  [page]
  (->> (s/select (s/descendant (s/child (s/and (s/tag :table) (s/class "tb-main-content"))
                                        (s/tag :tbody)
                                        (s/and (s/tag :tr) s/first-child)
                                        (s/tag :td)
                                        (s/tag :table))
                               (s/and (s/tag :p) (s/nth-child 5))
                               (s/tag :a))
                 page)
       (sequence
        (comp
         (remove #(#{["Next"] ["Previous"]} (:content %)))
         anchor-hrefs))))

(defn part-urls
  [wanted-list-id]
  (let [first-page (parse-url ((:wanted urls) wanted-list-id))
        other-page-urls (wanted-page-urls-on-wanted-page first-page)
        other-pages (map (comp parse-url #(str "http://www.bricklink.com/" %)) other-page-urls)]
    (mapcat part-urls-on-wanted-page (conj other-pages first-page))))
