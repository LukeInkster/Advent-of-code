(ns advent-2020.day-23
  (:require [clojure.string :as string]
            [common.util :refer :all]))


(defn read-cups
  [input]
  (mapv #(Integer/parseInt (str %)) input))


(defn play
  [cups rounds]
  (let [cnt (inc (count cups))
        possible-targets (map-to (fn [cup] (map (fn [x] (if (> 1 x) (mod (dec x) cnt) x))
                                                (range (dec cup) (- cup 5) -1)))
                                 cups)]
    (loop [[cup a b c & more] cups
           i 0]
      (if (= i rounds)
        (into [cup a b c] more)
        (let [target-val (first (remove #{a b c} (possible-targets cup)))
              [before [target & after]] (split-with (partial not= target-val) more)]
          (recur (concat before [target a b c] after [cup]) (inc i)))))))


(defn part-1
  [input]
  (let [cups (read-cups input)
        result (play cups 100)]
    (string/join "" (rest (take (count cups) (drop-while (partial not= 1) (cycle result)))))))


(defn part-2
  [input]
  (let [cups (take 1000000 (concat (read-cups input) (drop 10 (range))))
        ;; This takes ~40 seconds for 100 but needs to be able to do 10 million
        result (play cups 100)]
    (apply * (rest (take 3 (drop-while (partial not= 1) (cycle result)))))))


(def small-input
  "389125467")


(def large-input
  "643719258")
