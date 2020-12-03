(ns advent-2020.day-2
  (:require [clojure.string :as string]))


(defn input-string->rows
  [input]
  (map
    (fn [line]
      (let [[min max char pwd] (-> line
                                   (string/replace #"[-:]" " ")
                                   (string/split #"\s+"))]
        [(Integer/parseInt min) (Integer/parseInt max) (first (char-array char)) pwd]))
    (string/split-lines input)))


(defn part-one-password-valid?
  [min max char pwd]
  (<= min (count (filter #{char} pwd)) max))


(defn part-one
  [input]
  (count (filter (partial apply part-one-password-valid?) (input-string->rows input))))


(defn part-two-password-valid?
  [a b char pwd]
  (println a b char pwd (= 1 (count (filter (fn [idx] (= char (.charAt pwd (dec idx)))) [a b]))))
  (= 1 (count (filter (fn [idx] (= char (.charAt pwd (dec idx)))) [a b]))))


(defn part-two
  [input]
  (count (filter (partial apply part-two-password-valid?) (input-string->rows input))))
