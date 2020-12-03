(ns advent-2019.day-4)


(defn two-adjacent-match?
  [n]
  (some (partial apply =) (partition 2 1 (str n))))


(defn digits-increase?
  [n]
  (every? (partial apply <=) (partition 2 1 (map #(Integer/parseInt (str %)) (str n)))))


(defn valid-password-count-q1
  [lo hi]
  (count (filter (every-pred two-adjacent-match? digits-increase?)
                 (range lo (inc hi)))))


(defn exactly-two-adjacent-match?
  [n]
  (some (fn [[a b c d]] (and (= b c) (not= a b) (not= c d)))
        (partition 4 1 (str \- n \-))))


(defn valid-password-count-q2
  [lo hi]
  (count (filter (every-pred exactly-two-adjacent-match? digits-increase?)
                 (range lo (inc hi)))))
