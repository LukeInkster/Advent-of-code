(ns advent-2020.day-15
  (:require [clojure.string :as string]))


(defn run-game
  [input ^long end]
  (let [nums (mapv (fn [num] (Long/parseLong num)) (string/split input #","))]
    (loop [i (count nums)
           last-seen (into {} (map-indexed (fn [idx num] [num idx]) (butlast nums)))
           previous-num (long (last nums))]
      (if (= end i)
        previous-num
        (recur (unchecked-inc i)
               (assoc last-seen previous-num (dec i))
               (if (contains? last-seen previous-num)
                 (- (unchecked-dec i) (long (get last-seen previous-num)))
                 0))))))


(defn part-1
  [input]
  (run-game input 2020))


(defn part-2
  [input]
  (run-game input 30000000))


(def small-input
  "0,3,6")


(def large-input
  "12,1,16,3,11,0")
