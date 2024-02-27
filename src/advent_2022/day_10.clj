(ns advent-2022.day-10
  (:require [clojure.string :as string]))

(defn get-xs-by-tick
  [input-string]
  (loop [[instruction & more] (string/split-lines input-string)
         xs [1]]
    (cond
      (nil? instruction) xs
      (= "noop" instruction) (recur more (conj xs (last xs)))
      :else (let [amount (Integer/parseInt (get (string/split instruction #"\s") 1))]
              (recur more (into xs [(last xs) (+ amount (last xs))]))))))

(defn part-one
  [input-string]
  (let [xs (get-xs-by-tick input-string)
        indices (range 20 (inc (count xs)) 40)]
    (reduce + (map #(* (nth xs (dec %)) %) indices))))

(defn part-two
  [input-string]
  (let [xs (get-xs-by-tick input-string)]
    (mapv (fn [sprite pixel]
            (if (zero? pixel) (println))
            (print (if (<= -1 (- pixel sprite) 1) "#" " ")))
          xs
          (cycle (range 40)))))

(def small-input "noop\naddx 3\naddx -5")

(def medium-input "addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\nnoop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\naddx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9\naddx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop")

(def large-input "addx 1\naddx 4\nnoop\nnoop\nnoop\nnoop\naddx 6\naddx -1\nnoop\naddx 5\nnoop\naddx 5\nnoop\nnoop\nnoop\naddx 1\naddx 3\naddx 1\naddx 6\naddx -1\nnoop\nnoop\nnoop\naddx 7\nnoop\naddx -39\nnoop\nnoop\nnoop\naddx 7\naddx 3\naddx -2\naddx 2\nnoop\naddx 3\naddx 2\naddx 5\naddx 2\naddx -8\naddx 13\nnoop\naddx 3\naddx -2\naddx 2\naddx 5\naddx -31\naddx 36\naddx -2\naddx -36\nnoop\nnoop\nnoop\naddx 3\naddx 5\naddx 2\naddx -7\naddx 15\naddx -5\naddx 5\naddx 2\naddx 1\naddx 4\nnoop\naddx 3\nnoop\naddx 2\naddx -13\naddx -16\naddx 2\naddx 35\naddx -40\nnoop\nnoop\naddx 7\nnoop\nnoop\nnoop\naddx 5\nnoop\naddx 5\naddx 10\naddx -10\nnoop\nnoop\nnoop\naddx 3\nnoop\naddx 16\naddx -9\nnoop\nnoop\nnoop\naddx 3\nnoop\naddx 7\naddx -32\naddx 35\naddx -38\naddx 22\naddx 10\naddx -29\naddx 2\nnoop\naddx 3\naddx 5\naddx 2\naddx 2\naddx -12\naddx 13\nnoop\nnoop\naddx 7\naddx 5\nnoop\nnoop\nnoop\naddx 7\naddx -6\naddx 2\naddx 5\naddx -38\naddx 1\nnoop\nnoop\naddx 2\nnoop\naddx 3\naddx 5\nnoop\naddx 4\naddx -2\naddx 5\naddx 2\naddx 1\nnoop\naddx 4\naddx 4\naddx -14\naddx 16\nnoop\naddx -13\naddx 18\naddx -1\nnoop\nnoop\nnoop")

