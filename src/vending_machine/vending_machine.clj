(ns vending-machine.vending-machine
  (:require
    [schema.core :as s]
    [clojure.java.io :as io]
    [clojurewerkz.propertied.properties :as p])
  (:import (java.util Properties))
  (:import (java.io Reader)))

(s/set-fn-validation! true)

(s/defschema ListOfInt (s/maybe [s/Int]))

(s/defschema Map (s/pred map?))

(s/defn ^:private sanitize-coin-map :- Map
        [coins :- Map]
        (into (sorted-map-by <)
              (remove #(= (second %) 0))
              (for [[key value] coins]
                [(Integer. (clojure.string/replace (str key) #":" "")) value])))

(s/def denominations {:1   "One Penny"
                      :2   "Two Pence"
                      :5   "Five Pence"
                      :10  "Ten Pence"
                      :20  "Twenty Pence"
                      :50  "Fifty Pence"
                      :100 "One Pound"})

(s/defn ^:private stringify-inventory :- s/Str
        [coins :- Map]
        (apply str (for [[key value] coins]
                     (str (clojure.string/replace (str key) #":" "") "=" value "\n"))))

(s/defn ^:private ^:io update-coin-inventory
        [file-name :- s/Str
         coins :- Map]
        (spit file-name (stringify-inventory coins)))

(s/defn ^:private ^:io remove-coin :- Map
        [money-map :- Map
         value :- s/Int]
        (let [quantity (money-map value)]
          (condp = quantity
            nil (throw (RuntimeException.
                         (str "no coin with value " value " to remove")))
            1 (dissoc money-map value)
            (let [result (assoc money-map value (dec quantity))
                  _ (update-coin-inventory "resources/coin-inventory.properties" result)]
              result))))

(declare get-optimal-change*)

(s/defn ^:private make-optimal-change :- ListOfInt
        [value :- s/Int
         pence :- s/Int
         coins :- Map]
        (cond
          (zero? pence) '()
          (= value pence) (list value)
          (> value pence) nil
          true (conj (get-optimal-change* (- pence value) coins) value)))

(s/defn ^:private get-optimal-change* :- ListOfInt
        [pence :- s/Int
         coins :- Map]
        (some
          #(make-optimal-change % pence coins)
          (reverse (keys coins))))

(declare get-change*)

(s/defn ^:private make-change :- ListOfInt
        [value :- s/Int
         pence :- s/Int
         coins :- Map]
        (cond
          (zero? pence) '()
          (= value pence) (list value)
          (> value pence) nil
          true
          (let [new-amount (- pence value)
                new-coins-map (remove-coin coins value)
                change (get-change* new-amount new-coins-map)]
            (if change
              (conj change value)
              (throw (Exception. "Unsufficient Coinage"))))))

(s/defn ^:private get-change*
        [pence coins]
        (some
          #(make-change % pence coins)
          (reverse (keys coins))))

(defn- ^:io load-props
  [file-name]
  (with-open [^Reader reader (io/reader file-name)]
    (let [props (Properties.)]
      (.load props reader)
      (into {} (for [[key value] props] [(keyword key) (read-string value)])))))

(defprotocol VM-Protocol
  "vending machine proocol"
  (get-optimal-change [this pence])
  (get-change [this pence]))

(defrecord VendingMachine []
  VM-Protocol
  (get-optimal-change [this pence]
    (get-optimal-change* pence (sanitize-coin-map denominations)))
  (get-change [this pence]
    (get-change* pence (sanitize-coin-map (load-props "resources/coin-inventory.properties")))))