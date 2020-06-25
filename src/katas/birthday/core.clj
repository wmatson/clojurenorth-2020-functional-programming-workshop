(ns katas.birthday.core
  (:require
   [clojure.data.csv :as csv]
   [clojure.java.io :as io]
   [postal.core :as postal])
  (:import
   (java.time LocalDate)
   (java.time.format DateTimeFormatter)
   (java.time.temporal ChronoUnit)))

(def ^:private postal-host
  {:host "localhost"
   :user "azurediamond"
   :pass "hunter2"
   :port 2525})

(defn- parse-date [date-str]
  (LocalDate/parse date-str (DateTimeFormatter/ofPattern "yyyy/MM/dd")))

(defn- load-employees []
  (with-open [reader (io/reader (io/resource "birthday/employees.csv"))]
    (let [[headers & data] (csv/read-csv reader)
          kw-headers (map keyword headers)]
      (->> (mapv zipmap (repeat kw-headers) data)
           (map #(update % :date_of_birth parse-date))))))

(defn- get-today []
  (LocalDate/now))

(defn- same-day? [& days]
  (apply = (map #(vector (.getMonth %) (.getDayOfMonth %)) days)))

(defn- format-email [{:keys [employee today]}]
  {:from "me@example.com"
   :to (:email employee)
   :subject "Happy Birthday!"
   :body (str "Happy Birthday " (:name employee) "! "
              "Wow, you're "
              (.between ChronoUnit/YEARS
                        (:date_of_birth employee)
                        today)
              " already!")})

(defn to-greetings [today employees]
  (->> employees
       (filter #(same-day? today (:date_of_birth %)))
       (map #(format-email {:employee %
                            :today today}))))

(defn greet! []
  (doseq [greeting (to-greetings (get-today) (load-employees))]
    (postal/send-message
     postal-host
     greeting)))

(comment
  (load-employees)

  (.plusYears (get-today) 1)
  (.getDayOfYear (.plusYears (get-today) 1))
  (same-day? (get-today) (.plusYears (get-today) -4))
  (greet!))
