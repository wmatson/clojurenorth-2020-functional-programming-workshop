(ns katas.test.birthday
  (:require
    [clojure.test :refer [deftest is are testing]]
    [katas.birthday.core :as birthday])
  (:import [java.time LocalDate]
           [java.time.format DateTimeFormatter]))

(deftest same-day-test
  (testing "Non-leap-year-cases"
    (are [expected days]
         (= expected (apply #'birthday/same-day? days))
      true [(LocalDate/of 2019 01 01) (LocalDate/of 2019 01 01)]
      true [(LocalDate/of 2010 02 15) (LocalDate/of 2019 02 15)]
      true [(LocalDate/of 2018 01 01) (LocalDate/of 2019 01 01)]
      true [(LocalDate/of 2022 01 31) (LocalDate/of 2019 01 31)]
      true [(LocalDate/of 2021 02 28) (LocalDate/of 2019 02 28)]
      true [(LocalDate/of 2017 05 28) (LocalDate/of 2019 05 28)]
      true [(LocalDate/of 2019 12 31) (LocalDate/of 2053 12 31)]
      false [(LocalDate/of 2019 01 01) (LocalDate/of 2019 01 02)]))
  (testing "Non-leap-year-cases"
    (are [expected days]
         (= expected (apply #'birthday/same-day? days))
      true [(LocalDate/of 2020 01 01) (LocalDate/of 2020 01 01)]
      true [(LocalDate/of 2020 02 15) (LocalDate/of 2020 02 15)]
      true [(LocalDate/of 2018 01 01) (LocalDate/of 2020 01 01)]
      true [(LocalDate/of 2020 01 31) (LocalDate/of 2020 01 31)]
      true [(LocalDate/of 2019 02 28) (LocalDate/of 2020 02 28)]
      ; true [(LocalDate/of 1996 02 29) (LocalDate/of 2019 02 28)])]
      ; false [(LocalDate/of 1996 02 29) (LocalDate/of 2020 02 28)]
      true [(LocalDate/of 2020 05 28) (LocalDate/of 2020 05 28)]
      true [(LocalDate/of 2016 12 31) (LocalDate/of 2020 12 31)]
      false [(LocalDate/of 2020 01 01) (LocalDate/of 2020 01 02)])))

(defn- gen-employee [date-str id & [expected?]]
  (cond->
   {:name (str "Employee " id) 
    :date_of_birth (LocalDate/parse date-str (DateTimeFormatter/ofPattern "yyyy-MM-dd"))
    :email (str "e" id "@corp.com")}
   expected? (assoc :expected true)))

(deftest greeting-correct-individuals?
  (let [today (LocalDate/of 2020 05 06)
        emps [(gen-employee "1976-01-01" 1)
              (gen-employee "1234-05-06" 2 true)
              (gen-employee "1985-05-06" 3 true)
              (gen-employee "1991-05-06" 4 true)
              (gen-employee "1992-05-04" 5)
     ;;Should future employees get birthday emails?
              (gen-employee "3500-05-06" 6 true)]
        expected-emails (->> emps
                             (filter :expected)
                             (map :email)
                             set)
        actual-emails (->> emps
                           (birthday/to-greetings today)
                           (map :to)
                           set)]
    (is (= expected-emails actual-emails))))

(deftest greet!
  (testing "sends email to everyone with birthday today"
    ;; How do we test???
    #_(greet!)))
