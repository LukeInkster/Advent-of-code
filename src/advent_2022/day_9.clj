(ns advent-2022.day-9
  (:require [clojure.string :as string]))

(def x 0)
(def y 1)

(defn update-nodes
  [[head & other-nodes] move]
  (let [new-head (case move
                   "U" [(head x) (inc (head y))]
                   "D" [(head x) (dec (head y))]
                   "L" [(dec (head x)) (head y)]
                   "R" [(inc (head x)) (head y)])]

    (loop [[this & other-nodes] other-nodes
           new-nodes [new-head]]
      (if this
        (let [previous (last new-nodes)

              should-move? (or (>= (- (previous x) (this x)) 2)
                               (<= (- (previous x) (this x)) -2)
                               (>= (- (previous y) (this y)) 2)
                               (<= (- (previous y) (this y)) -2))

              new-x (cond
                      (>= (- (previous x) (this x)) 1) (inc (this x))
                      (<= (- (previous x) (this x)) -1) (dec (this x))
                      :else (this x))

              new-y (cond
                      (>= (- (previous y) (this y)) 1) (inc (this y))
                      (<= (- (previous y) (this y)) -1) (dec (this y))
                      :else (this y))]
          (recur other-nodes (conj new-nodes (if should-move? [new-x new-y] this))))
        new-nodes))))

(defn simulate-string
  [input-string length]
  (let [moves (mapcat #(let [[dir times] (string/split % #"\s")]
                         (repeat (Integer/parseInt times) dir))
                      (string/split-lines input-string))]
    (count (loop [[move & remaining-moves] moves
                  nodes (vec (repeat length [0 0]))
                  tail-history #{[0 0]}]
             (if move
               (let [new-nodes (update-nodes nodes move)]
                 (recur remaining-moves new-nodes (conj tail-history (last new-nodes))))
               tail-history)))))

(defn part-one [input-string] (simulate-string input-string 2))

(defn part-two [input-string] (simulate-string input-string 10))

(def small-input "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2")

(def medium-input "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20")

(def large-input "L 2\nU 2\nR 2\nL 1\nR 1\nU 1\nD 1\nL 1\nR 1\nD 2\nR 1\nU 2\nD 2\nL 2\nR 2\nU 1\nL 1\nD 2\nU 2\nL 1\nU 2\nL 2\nU 2\nL 2\nD 1\nU 2\nD 1\nR 1\nU 2\nR 2\nD 1\nR 1\nU 1\nR 2\nL 1\nR 1\nL 1\nD 1\nL 1\nD 2\nU 1\nL 2\nR 2\nU 1\nR 1\nL 1\nU 2\nD 1\nL 2\nD 2\nR 1\nL 2\nU 1\nR 1\nL 1\nR 2\nD 2\nR 2\nL 1\nR 1\nU 1\nL 2\nD 2\nL 2\nR 2\nU 2\nD 2\nL 1\nU 2\nD 2\nL 2\nD 1\nU 2\nD 2\nU 2\nR 2\nU 2\nD 2\nL 1\nR 2\nL 1\nU 1\nD 1\nR 1\nL 1\nU 1\nR 2\nL 1\nD 2\nR 1\nD 1\nR 2\nD 2\nL 2\nU 1\nR 1\nD 2\nL 1\nU 1\nD 1\nL 1\nD 2\nR 1\nD 1\nR 2\nD 2\nR 1\nL 1\nR 1\nD 1\nR 1\nU 1\nL 3\nD 3\nR 2\nU 3\nR 1\nD 3\nU 1\nR 3\nL 2\nU 3\nL 3\nD 2\nR 2\nL 3\nD 3\nU 1\nL 3\nD 1\nU 3\nL 3\nD 1\nR 2\nD 1\nR 1\nU 2\nR 2\nU 2\nR 1\nL 2\nR 2\nL 2\nD 1\nU 1\nR 3\nL 3\nD 1\nU 3\nL 1\nU 2\nR 1\nL 1\nR 2\nU 1\nR 3\nD 1\nL 3\nD 3\nL 3\nR 3\nD 1\nR 1\nL 2\nD 2\nR 3\nL 1\nD 1\nU 1\nD 1\nU 2\nR 3\nU 2\nD 3\nR 3\nD 2\nL 1\nR 1\nD 2\nR 1\nL 2\nD 2\nU 2\nL 1\nR 2\nU 1\nD 2\nR 2\nD 2\nL 2\nR 3\nD 1\nU 2\nL 2\nR 3\nD 3\nL 3\nU 3\nL 2\nD 1\nR 1\nL 3\nD 2\nR 2\nD 2\nL 1\nU 1\nR 2\nU 3\nD 1\nL 1\nR 1\nL 2\nU 2\nL 3\nU 2\nD 2\nU 1\nL 1\nR 1\nU 3\nD 3\nR 4\nL 4\nU 1\nD 2\nL 1\nD 4\nL 3\nD 1\nR 4\nU 2\nD 2\nL 1\nD 3\nL 3\nR 2\nL 2\nR 1\nL 3\nR 1\nL 2\nU 1\nL 2\nU 4\nD 4\nU 4\nL 4\nR 4\nD 3\nL 1\nR 3\nD 4\nR 1\nL 4\nR 4\nU 1\nR 3\nU 2\nD 4\nL 1\nU 3\nL 2\nR 2\nD 1\nL 1\nU 3\nL 2\nU 1\nD 1\nL 4\nD 4\nU 2\nD 4\nR 3\nU 1\nR 1\nD 3\nL 1\nD 2\nR 2\nD 3\nL 3\nU 4\nL 4\nR 1\nD 2\nL 3\nU 3\nD 1\nR 4\nL 4\nU 4\nR 3\nL 3\nD 3\nL 1\nD 1\nU 1\nL 3\nR 4\nL 3\nR 4\nL 1\nD 1\nR 4\nU 3\nD 4\nU 1\nD 3\nR 4\nU 2\nD 1\nL 3\nD 3\nL 1\nR 3\nU 2\nL 4\nD 4\nL 2\nD 4\nU 1\nL 3\nR 2\nD 2\nU 2\nD 2\nR 1\nU 3\nR 3\nU 4\nL 3\nR 2\nD 4\nU 3\nD 4\nR 5\nL 1\nD 3\nL 5\nR 3\nD 5\nU 3\nR 1\nU 3\nD 4\nR 1\nD 5\nR 3\nL 4\nD 2\nR 2\nU 2\nL 4\nU 3\nD 1\nL 2\nD 3\nU 2\nD 2\nR 4\nL 5\nU 1\nD 5\nR 2\nD 5\nR 3\nL 3\nR 4\nD 1\nL 5\nD 2\nU 2\nD 4\nU 4\nR 4\nL 2\nD 3\nL 3\nR 3\nL 4\nD 2\nU 1\nR 3\nL 3\nR 2\nL 5\nR 4\nD 2\nL 3\nR 2\nU 2\nR 4\nL 4\nR 1\nU 5\nD 5\nU 1\nR 4\nL 5\nR 5\nL 3\nR 1\nU 2\nL 4\nU 4\nL 5\nD 1\nU 5\nD 5\nU 2\nR 4\nU 3\nD 5\nR 1\nD 1\nL 5\nR 4\nL 4\nR 3\nD 3\nU 2\nR 5\nL 5\nU 5\nL 3\nD 2\nR 3\nL 4\nR 1\nL 3\nU 5\nL 2\nD 5\nR 5\nL 3\nD 3\nU 3\nL 5\nU 3\nR 1\nU 3\nL 5\nD 1\nU 4\nR 6\nU 1\nD 3\nU 1\nL 5\nR 1\nU 3\nD 3\nR 3\nU 5\nL 1\nR 2\nL 3\nU 5\nL 2\nR 6\nD 3\nU 3\nR 2\nU 5\nL 2\nU 5\nD 5\nR 1\nD 1\nU 6\nL 2\nD 2\nL 4\nR 5\nU 1\nD 3\nR 4\nD 1\nU 6\nR 4\nD 4\nU 6\nR 5\nL 5\nD 6\nR 6\nU 2\nL 2\nD 3\nR 6\nU 5\nL 5\nR 6\nL 3\nD 5\nU 2\nL 3\nU 4\nD 2\nU 5\nR 6\nD 3\nU 4\nL 2\nU 5\nL 6\nR 6\nD 5\nR 4\nD 3\nL 1\nD 4\nR 6\nL 4\nR 3\nU 5\nR 6\nU 1\nD 3\nL 4\nD 2\nU 6\nL 4\nD 6\nL 4\nD 5\nL 6\nD 1\nU 5\nR 5\nL 5\nR 4\nD 2\nR 2\nU 3\nD 2\nR 2\nD 2\nR 6\nU 6\nL 4\nU 6\nR 5\nD 5\nU 2\nR 4\nL 4\nD 2\nR 1\nD 4\nU 5\nR 4\nD 1\nU 2\nL 1\nR 1\nU 4\nR 3\nL 6\nD 5\nL 6\nR 1\nD 3\nU 4\nD 3\nL 2\nD 7\nU 2\nD 7\nU 4\nL 3\nR 6\nD 1\nL 6\nR 3\nL 6\nU 7\nR 2\nD 3\nL 3\nU 1\nL 4\nU 1\nL 1\nD 3\nR 1\nD 5\nL 3\nR 1\nD 4\nU 1\nR 4\nD 7\nU 7\nD 1\nR 7\nD 3\nR 2\nD 1\nU 6\nR 4\nU 7\nD 1\nU 2\nL 5\nR 2\nU 5\nL 1\nU 5\nD 6\nR 5\nD 4\nR 6\nU 3\nL 2\nD 6\nR 1\nD 1\nU 3\nL 2\nU 2\nL 5\nD 5\nL 7\nD 6\nR 3\nD 6\nU 6\nL 7\nU 4\nL 7\nD 7\nL 7\nU 5\nR 3\nL 5\nD 4\nR 3\nD 4\nU 7\nL 7\nD 6\nU 1\nR 5\nL 4\nR 5\nD 7\nR 4\nL 5\nU 5\nR 3\nU 6\nD 4\nU 7\nR 7\nD 1\nR 6\nU 6\nD 3\nL 1\nR 1\nU 5\nR 2\nL 6\nU 2\nR 8\nU 3\nR 3\nL 8\nR 4\nU 4\nL 4\nR 2\nL 2\nU 1\nD 2\nU 6\nL 1\nR 6\nU 2\nR 1\nD 2\nR 8\nD 6\nR 3\nL 1\nD 6\nR 3\nU 3\nL 3\nR 1\nD 4\nR 3\nD 8\nU 4\nL 1\nD 7\nL 2\nR 6\nU 2\nL 6\nU 7\nD 4\nR 2\nL 5\nR 3\nL 1\nU 2\nL 8\nR 8\nU 2\nR 8\nL 1\nU 8\nL 1\nD 4\nR 5\nU 1\nR 5\nL 8\nD 8\nR 4\nL 7\nU 3\nD 8\nL 7\nR 7\nL 8\nD 2\nU 2\nD 8\nL 1\nR 3\nL 7\nD 4\nR 5\nL 4\nR 7\nU 5\nL 7\nU 4\nR 4\nU 5\nR 3\nD 7\nU 7\nR 8\nL 7\nU 4\nD 2\nR 1\nD 8\nU 4\nD 7\nR 1\nD 4\nL 6\nU 1\nL 8\nD 1\nR 7\nL 2\nU 7\nR 7\nL 4\nU 6\nL 5\nD 3\nR 3\nL 1\nR 2\nU 3\nD 3\nR 5\nU 6\nD 8\nL 7\nR 2\nL 6\nR 4\nL 4\nR 5\nL 8\nR 8\nU 7\nL 6\nR 9\nD 2\nL 2\nD 9\nU 3\nD 3\nL 1\nU 9\nD 7\nU 3\nD 8\nR 3\nU 3\nL 7\nD 9\nL 8\nU 8\nL 6\nR 7\nD 1\nU 1\nR 5\nL 3\nR 7\nU 8\nR 8\nD 5\nL 4\nR 5\nD 6\nL 7\nU 7\nR 9\nU 4\nR 5\nL 9\nR 7\nD 7\nU 3\nD 7\nL 3\nU 5\nD 2\nU 8\nD 1\nU 7\nR 7\nD 2\nU 8\nD 4\nR 7\nL 3\nU 3\nD 2\nR 9\nU 4\nD 4\nU 1\nL 4\nU 2\nD 7\nU 1\nL 4\nR 5\nL 3\nU 8\nR 4\nL 6\nU 6\nR 5\nU 3\nL 6\nD 7\nU 5\nR 2\nD 3\nL 6\nU 5\nL 7\nU 4\nR 6\nD 2\nL 1\nR 7\nL 1\nD 9\nL 6\nD 1\nR 1\nU 7\nR 4\nL 9\nD 9\nU 4\nD 8\nU 1\nL 8\nD 2\nU 9\nL 7\nR 10\nU 10\nL 10\nD 5\nR 9\nD 8\nL 1\nR 3\nL 7\nU 2\nL 1\nD 4\nR 5\nU 5\nD 1\nR 3\nD 3\nR 7\nU 2\nD 6\nU 6\nL 1\nD 7\nR 5\nL 9\nR 1\nU 7\nD 10\nU 3\nD 5\nU 8\nL 5\nR 8\nL 2\nR 9\nL 10\nR 9\nD 6\nU 1\nD 2\nL 7\nR 5\nL 6\nR 2\nD 9\nU 4\nL 8\nD 6\nL 10\nD 3\nU 8\nD 9\nU 5\nL 1\nU 9\nR 2\nD 5\nL 7\nU 4\nD 10\nU 6\nD 7\nU 6\nL 3\nD 2\nR 6\nL 10\nR 7\nU 4\nL 6\nD 1\nR 3\nD 8\nU 5\nL 7\nU 4\nL 6\nU 5\nL 3\nD 4\nU 4\nL 4\nD 6\nU 7\nL 10\nD 5\nL 7\nD 3\nU 3\nL 1\nD 3\nU 8\nD 2\nR 5\nU 7\nD 3\nL 5\nD 2\nL 2\nR 7\nL 10\nD 2\nU 2\nR 2\nD 9\nR 4\nL 4\nR 9\nD 8\nL 1\nU 8\nL 8\nR 4\nL 9\nU 11\nL 3\nR 4\nU 6\nL 11\nU 8\nR 4\nD 9\nR 5\nD 9\nU 11\nD 4\nR 1\nU 6\nL 8\nR 10\nD 11\nU 6\nD 4\nU 2\nD 7\nL 3\nR 5\nL 7\nR 5\nL 7\nD 2\nL 7\nU 9\nD 3\nL 6\nR 2\nU 6\nL 8\nR 10\nU 3\nR 7\nL 8\nU 1\nD 4\nL 1\nU 1\nD 6\nL 5\nU 4\nR 3\nU 7\nR 5\nU 11\nL 11\nR 11\nL 2\nR 3\nU 2\nR 10\nL 4\nU 8\nL 6\nR 8\nL 11\nU 6\nD 2\nL 7\nD 10\nR 3\nL 6\nR 8\nD 4\nU 7\nD 4\nR 1\nD 6\nR 11\nD 11\nR 9\nU 10\nR 7\nD 8\nU 4\nL 3\nR 9\nU 2\nD 3\nL 7\nR 10\nL 9\nD 11\nU 5\nL 10\nD 2\nR 10\nD 7\nU 4\nR 8\nD 9\nL 3\nR 4\nL 11\nD 4\nU 5\nR 7\nU 3\nR 9\nD 11\nR 1\nD 10\nR 2\nU 8\nL 7\nR 9\nU 9\nD 11\nR 6\nL 2\nD 4\nR 4\nD 6\nL 12\nD 11\nR 5\nU 1\nR 9\nL 9\nD 7\nL 1\nR 6\nL 4\nU 4\nR 10\nD 12\nR 1\nU 8\nD 8\nL 1\nU 12\nD 2\nR 7\nU 12\nL 10\nD 12\nR 6\nU 10\nD 8\nL 8\nU 1\nR 4\nL 7\nD 1\nL 7\nR 7\nU 4\nL 12\nU 6\nD 1\nU 8\nR 12\nL 11\nD 12\nR 3\nD 11\nU 2\nR 9\nD 11\nL 9\nR 1\nU 7\nD 9\nU 8\nL 8\nD 11\nR 9\nU 5\nD 6\nU 8\nD 6\nU 8\nD 11\nL 3\nD 4\nR 1\nU 6\nL 10\nU 11\nD 11\nU 5\nD 1\nR 3\nD 2\nL 4\nR 4\nD 10\nL 3\nD 12\nU 5\nR 1\nU 9\nD 1\nU 2\nR 7\nL 7\nU 8\nL 9\nD 4\nU 6\nR 9\nU 4\nL 11\nU 5\nL 6\nD 4\nU 8\nL 11\nU 2\nL 4\nU 1\nD 9\nR 2\nL 5\nD 8\nR 11\nU 7\nL 9\nD 2\nU 12\nR 13\nD 1\nL 10\nD 5\nR 9\nU 2\nL 10\nU 12\nR 4\nU 11\nD 4\nL 3\nU 12\nD 9\nR 9\nL 2\nD 5\nL 2\nU 6\nD 6\nL 1\nU 8\nR 9\nD 1\nL 11\nR 4\nD 4\nU 7\nR 9\nU 1\nL 5\nD 6\nU 2\nD 8\nU 6\nL 2\nD 11\nL 2\nD 11\nU 12\nD 6\nU 12\nR 13\nD 13\nL 6\nU 7\nL 1\nD 12\nU 9\nL 2\nU 13\nR 3\nU 8\nL 9\nD 13\nR 4\nU 4\nD 2\nL 7\nR 1\nD 1\nL 4\nR 12\nU 2\nD 12\nL 11\nU 10\nL 5\nU 9\nD 11\nR 7\nL 4\nD 10\nL 10\nU 5\nR 8\nL 5\nR 12\nL 9\nD 3\nU 11\nL 4\nR 8\nU 8\nR 9\nD 11\nU 10\nL 11\nU 4\nD 6\nR 3\nU 3\nR 13\nL 5\nD 9\nU 5\nR 1\nL 4\nD 9\nU 10\nR 8\nD 3\nL 7\nR 5\nU 2\nR 11\nU 12\nD 12\nR 3\nL 5\nD 13\nU 6\nR 5\nU 11\nD 12\nL 8\nU 14\nL 2\nR 4\nU 7\nR 9\nL 10\nU 4\nL 4\nR 12\nD 13\nR 6\nL 12\nD 6\nR 6\nD 9\nL 1\nU 8\nL 11\nD 2\nL 14\nR 10\nD 1\nU 8\nD 13\nL 3\nR 10\nU 4\nD 12\nR 12\nU 4\nD 2\nL 4\nU 6\nD 1\nU 7\nR 11\nU 7\nD 10\nR 6\nU 4\nD 2\nU 1\nL 2\nR 9\nD 3\nR 8\nD 12\nL 11\nU 2\nL 3\nU 5\nD 6\nL 14\nR 9\nL 14\nR 12\nU 12\nR 8\nL 13\nU 6\nL 10\nD 5\nU 14\nD 11\nL 11\nR 8\nL 6\nD 13\nR 5\nD 6\nL 7\nR 6\nD 8\nL 7\nD 9\nL 9\nU 8\nD 11\nL 7\nU 2\nD 9\nL 14\nR 3\nL 1\nD 9\nL 9\nR 1\nL 9\nU 6\nL 13\nD 7\nL 8\nD 2\nL 11\nU 8\nL 9\nU 7\nD 14\nL 11\nR 10\nD 1\nR 3\nD 9\nU 13\nD 3\nU 4\nR 2\nL 6\nU 11\nR 3\nL 9\nD 15\nR 12\nD 4\nL 14\nU 11\nL 2\nR 1\nL 10\nU 5\nR 11\nU 13\nR 13\nU 14\nD 2\nU 5\nD 14\nR 9\nD 8\nR 13\nL 1\nU 2\nR 7\nU 5\nL 8\nU 9\nR 3\nU 8\nR 9\nL 7\nR 15\nL 3\nD 12\nU 14\nR 12\nL 1\nR 9\nU 5\nR 4\nU 1\nD 4\nR 12\nL 2\nD 7\nU 2\nD 14\nU 2\nR 5\nD 12\nU 2\nD 5\nR 11\nL 5\nR 8\nD 10\nR 11\nL 12\nD 11\nL 6\nR 14\nD 2\nL 8\nR 6\nL 12\nR 13\nL 7\nR 4\nU 4\nD 15\nR 14\nD 5\nL 12\nU 7\nL 5\nD 5\nL 11\nU 10\nD 10\nL 13\nD 8\nR 10\nU 1\nL 6\nU 3\nL 12\nD 15\nU 12\nL 13\nU 7\nR 1\nL 13\nD 13\nU 2\nD 15\nU 2\nD 4\nL 7\nU 11\nR 9\nD 11\nL 9\nD 10\nU 9\nD 14\nR 8\nD 13\nU 16\nD 14\nR 13\nD 3\nL 9\nR 5\nD 2\nR 4\nD 14\nU 7\nD 5\nR 8\nL 14\nR 7\nU 4\nL 4\nR 15\nL 1\nU 12\nR 8\nL 4\nR 14\nL 13\nD 11\nU 3\nR 11\nD 11\nR 2\nD 10\nR 4\nD 8\nL 10\nR 3\nL 4\nR 16\nU 8\nD 9\nR 8\nD 15\nU 16\nR 15\nD 7\nR 10\nL 7\nR 2\nU 3\nR 16\nD 13\nL 10\nU 13\nD 8\nU 15\nD 3\nU 3\nL 16\nU 3\nR 11\nD 6\nR 12\nU 11\nR 2\nD 1\nU 3\nL 7\nU 9\nD 12\nR 13\nL 13\nU 5\nD 16\nL 14\nD 13\nU 15\nD 3\nU 15\nR 11\nD 16\nR 7\nD 2\nL 14\nR 8\nL 10\nR 3\nL 2\nU 2\nL 9\nD 5\nL 2\nD 12\nR 3\nU 7\nR 4\nU 2\nL 9\nR 13\nL 7\nU 16\nR 4\nD 1\nU 7\nR 3\nU 9\nL 6\nU 6\nD 7\nU 8\nD 16\nL 14\nU 16\nD 8\nU 6\nL 8\nD 2\nR 6\nU 14\nL 16\nR 7\nL 15\nU 15\nD 12\nR 2\nU 15\nR 8\nD 9\nR 6\nU 9\nD 1\nR 14\nU 7\nD 12\nR 7\nU 14\nR 16\nD 5\nR 11\nL 8\nU 4\nL 7\nD 8\nR 10\nD 9\nU 4\nL 5\nU 6\nD 17\nL 1\nU 16\nL 16\nR 14\nD 14\nU 3\nL 10\nD 3\nR 10\nU 2\nD 17\nR 12\nL 14\nD 10\nL 16\nU 14\nR 3\nL 17\nD 5\nU 14\nR 15\nD 5\nL 2\nR 6\nU 2\nL 5\nR 2\nL 10\nR 14\nL 16\nU 7\nL 5\nR 12\nL 13\nU 5\nL 13\nR 10\nL 12\nU 5\nR 15\nD 1\nU 12\nR 8\nD 12\nR 14\nU 7\nR 3\nL 15\nU 9\nD 10\nU 3\nR 17\nL 11\nD 4\nL 17\nR 5\nL 10\nR 3\nU 4\nL 3\nD 5\nR 1\nD 11\nL 9\nR 1\nL 9\nD 11\nL 1\nD 2\nU 6\nL 15\nU 1\nL 9\nD 11\nU 18\nD 16\nL 1\nR 1\nL 12\nU 3\nL 7\nR 14\nD 11\nR 9\nD 10\nL 7\nD 2\nL 10\nD 18\nR 13\nD 10\nR 16\nD 6\nU 8\nD 12\nR 6\nL 7\nD 16\nL 16\nU 11\nR 13\nD 12\nR 3\nD 2\nU 8\nL 9\nR 12\nL 11\nD 2\nL 17\nD 10\nU 12\nR 7\nD 15\nU 5\nL 10\nR 1\nU 13\nL 12\nR 7\nD 17\nU 5\nD 13\nU 13\nR 4\nU 14\nL 16\nR 18\nU 17\nL 16\nU 13\nR 5\nL 6\nU 6\nD 10\nR 6\nU 10\nL 8\nD 8\nL 3\nR 4\nU 17\nR 11\nD 14\nU 5\nL 17\nR 3\nU 6\nD 3\nR 11\nU 15\nD 4\nR 2\nD 9\nU 13\nL 12\nR 14\nL 17\nD 4\nU 14\nL 4\nU 11\nR 11\nL 6\nR 15\nL 18\nD 8\nU 2\nR 17\nD 10\nU 15\nD 18\nL 1\nD 6\nR 5\nD 14\nL 10\nD 16\nL 7\nD 15\nL 15\nD 15\nL 6\nU 4\nD 15\nU 17\nL 13\nR 10\nU 15\nD 16\nU 9\nR 9\nD 16\nR 12\nU 16\nD 11\nL 9\nR 5\nL 14\nR 1\nL 3\nD 4\nR 4\nD 5\nU 13\nD 14\nL 9\nU 4\nR 19\nD 3\nR 13\nU 17\nR 2\nD 13\nR 14\nL 18\nD 12\nL 5\nD 11\nU 19\nD 14\nU 2\nR 1\nU 18\nD 1\nL 9\nR 17\nU 10\nL 9\nD 15\nL 15\nU 15\nR 19\nD 17\nR 19\nD 9\nL 11\nU 16\nD 10\nU 6\nD 17\nR 3\nD 19\nR 13\nL 16\nR 2\nD 9\nR 3\nL 7\nR 18\nL 6\nU 5\nD 15\nL 5\nD 19\nR 6\nD 14\nU 16\nD 9\nL 18\nU 19\nL 10\nD 13\nR 18\nU 8\nL 5\nR 13\nU 15\nR 6\nD 12\nR 10\nD 5\nU 17\nR 9\nU 2\nD 14\nR 18\nD 3\nL 7\nD 5\nR 15\nU 2\nL 6\nR 5\nD 17\nL 19\nR 14\nU 13\nD 16\nU 1\nD 18\nL 14\nU 4\nR 8\nL 16\nD 15\nU 8")

