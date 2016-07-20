(ns vending-machine.vending-machine-test
  (:require
    [clojure.test :refer :all]
    [vending-machine.vending-machine :as vm]))

(def generate-coin-inventory
  (spit "resources/coin-inventory.properties" "100=11\n50=24\n20=0\n10=99\n5=200\n2=11\n1=23"))

(deftest optimal-change-test
  (let [fixtures [{:name "no change give test"
                   :in   {:pence 0}
                   :out  []}
                  {:name "1p test"
                   :in   {:pence 50}
                   :out  [50]}
                  {:name "50p test"
                   :in   {:pence 50}
                   :out  [50]}
                  {:name "75p test"
                   :in   {:pence 75}
                   :out  [50 20 5]}
                  {:name "£1.11 test"
                   :in   {:pence 111}
                   :out  [100 10 1]}]]
    (doseq [{{:keys [pence]} :in, expected-out :out} fixtures]
      (let [actual-out (vm/get-optimal-change (vm/->VendingMachine) pence)]
        (is (= expected-out actual-out))))))

(deftest change-test
  generate-coin-inventory
  (let [fixtures [{:name "no change give test"
                   :in   {:pence 0}
                   :out  []}
                  {:name "50p test"
                   :in   {:pence 20}
                   :out  [10 10]}
                  {:name "50p test"
                   :in   {:pence 50}
                   :out  [50]}
                  {:name "75p test"
                   :in   {:pence 75}
                   :out  [50 10 10 5]}
                  {:name "£1.11 test"
                   :in   {:pence 111}
                   :out  [100 10 1]}
                  {:name "£11.00 test"
                   :in   {:pence 1100}
                   :out  [100 100 100 100 100 100 100 100 100 100 50 50]}
                  {:name "£100"
                   :in   {:pence 10000}
                   :out  "Unsufficient Coinage"}]]
    (doseq [{{:keys [pence]} :in, expected-out :out} fixtures]
      (let [actual-out (try
                        (vm/get-change (vm/->VendingMachine) pence)
                        (catch Exception e (.getMessage e)))]
        (is (= expected-out actual-out))))))
