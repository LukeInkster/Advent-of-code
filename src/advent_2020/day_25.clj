(ns advent-2020.day-25
  (:require [clojure.string :as string]
            [common.util :refer :all]))


(defn nums
  [subject-number]
  (iterate (fn [value] (rem (* value subject-number) 20201227)) 1))


(defn part-1
  [input]
  (let [[card-key door-key] (map #(Long/parseLong %) (string/split-lines input))
        card-loops (count (take-while #(not= card-key %) (nums 7)))]
    (nth (nums door-key) card-loops)))


(def small-input
  "5764801\n17807724")


(def large-input
  "16616892\n14505727")
