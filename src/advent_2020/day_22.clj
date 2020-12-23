(ns advent-2020.day-22
  (:require [clojure.string :as string]
            [common.util :refer :all]))


(defn read-deck
  [deck-str]
  (mapv #(Long/parseLong %) (rest (string/split-lines deck-str))))


(defn read-decks
  [input]
  (mapv read-deck (string/split input #"\n\n")))


(defn play-part-1
  [[deck-1 deck-2]]
  (loop [d1 deck-1
         d2 deck-2]
    (cond (empty? d1) d2
          (empty? d2) d1
          :else (if (> (first d1) (first d2))
                  (recur (conj (subvec d1 1) (first d1) (first d2)) (subvec d2 1))
                  (recur (subvec d1 1) (conj (subvec d2 1) (first d2) (first d1)))))))


(defn part-1
  [input]
  (let [decks (read-decks input)
        winner-deck (play-part-1 decks)]
    (reduce + (map-indexed * (cons 0 (reverse winner-deck))))))


(defn play-part-2
  [[deck-1 deck-2]]
  (loop [d1 deck-1
         d2 deck-2
         seen-before? #{}]
    (let [c1 (first d1)
          c2 (first d2)
          new-seen-before (conj seen-before? [d1 d2])]
      (cond (seen-before? [d1 d2]) [1 d1]

            (nil? c2) [1 d1]

            (nil? c1) [2 d2]

            (and (> (count d1) c1) (> (count d2) c2))
            (let [[winner] (play-part-2 [(subvec d1 1 (inc c1)) (subvec d2 1 (inc c2))])]
              (if (= 1 winner)
                (recur (conj (subvec d1 1) (first d1) (first d2)) (subvec d2 1) new-seen-before)
                (recur (subvec d1 1) (conj (subvec d2 1) (first d2) (first d1)) new-seen-before)))

            (> c1 c2) (recur (conj (subvec d1 1) (first d1) (first d2)) (subvec d2 1) new-seen-before)

            :else (recur (subvec d1 1) (conj (subvec d2 1) (first d2) (first d1)) new-seen-before)))))


(defn part-2
  [input]
  (let [decks (read-decks input)
        [_ winning-deck] (play-part-2 decks)]
    (reduce + (map-indexed * (cons 0 (reverse winning-deck))))))


(def small-input
  "Player 1:\n9\n2\n6\n3\n1\n\nPlayer 2:\n5\n8\n4\n7\n10")


(def small-input-2
  "Player 1:\n43\n19\n\nPlayer 2:\n2\n29\n14")


(def large-input
  "Player 1:\n18\n50\n9\n4\n25\n37\n39\n40\n29\n6\n41\n28\n3\n11\n31\n8\n1\n38\n33\n30\n42\n15\n26\n36\n43\n\nPlayer 2:\n32\n44\n19\n47\n12\n48\n14\n2\n13\n10\n35\n45\n34\n7\n5\n17\n46\n21\n24\n49\n16\n22\n20\n27\n23")
