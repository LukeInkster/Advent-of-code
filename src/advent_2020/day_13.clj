(ns advent-2020.day-13
  (:require [clojure.string :as string]))


(defn decode-part-1
  [input]
  (let [[start-str ids-str] (string/split-lines input)]
    [(Integer/parseInt start-str)
     (keep (fn [id] (when-not (= "x" id) (Integer/parseInt id)))
           (string/split ids-str #","))]))


(defn part-1
  [input]
  (let [[start ids] (decode-part-1 input)]
    (->> (map (fn [id] [id (- id (mod start id))]) ids)
         (apply min-key second)
         (apply *))))


(defn decode-part-2
  [input]
  (keep-indexed
    (fn [idx id] (when-not (= "x" id) [idx (Integer/parseInt id)]))
    (string/split (second (string/split-lines input)) #",")))


(defn filter-cycle
  "prior-cycles is the times all previously checked busses intersected. This function filters that
  down to the elements of prior-cycles which also intersect with the current busses idx and id"
  [prior-cycles [idx id]]
  (let [;; Shift prior cycles by idx so they should line up exactly with the current bus
        shifted-prior-cycles (map #(+ % idx) prior-cycles)
        ;; Find the first two times where this bus is "idx" minutes apart from prior cycles
        [t1 t2] (filter #(zero? (mod % id)) shifted-prior-cycles)
        ;; Remove the shift
        cycle-start (- t1 idx)]
    ;; The other times where they are "idx" minutes apart will have the same spacing so make an iterator
    (iterate #(+ % (- t2 t1)) cycle-start)))


(defn part-2
  [input]
  (let [parsed-input (decode-part-2 input)
        [[idx id] & more-buses] parsed-input
        initial-cycle (iterate (partial + id) idx)]
    (first (reduce filter-cycle initial-cycle more-buses))))


(def small-input
  "939\n7,13,x,x,59,x,31,19")


(def small-input-2
  "0\n17,x,13,19")


(def small-input-3
  "0\n67,7,59,61")


(def small-input-4
  "0\n67,x,7,59,61")


(def small-input-5
  "0\n67,7,x,59,61")


(def small-input-6
  "0\n1789,37,47,1889")


(def large-input
  "1000186\n17,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,907,x,x,x,x,x,x,x,x,x,x,x,19,x,x,x,x,x,x,x,x,x,x,23,x,x,x,x,x,29,x,653,x,x,x,x,x,x,x,x,x,41,x,x,13")
