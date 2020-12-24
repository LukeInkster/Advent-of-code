(ns advent-2020.day-23
  (:require [clojure.string :as string]
            [common.util :refer :all]))


(defn read-cups
  [input]
  (mapv #(Integer/parseInt (str %)) input))


(defn play
  [cups rounds]
  (let [cup->next (into {(last cups) (first cups)} (map vec) (partition 2 1 cups))
        highest (apply max (vals cup->next))
        possible-targets (map-to (fn [cup] (map (fn [x] (if (< x 1) (mod (dec x) (inc highest)) x))
                                                (range (dec cup) (- cup 5) -1)))
                                 (keys cup->next))]
    (loop [current (first cups)
           cup->next cup->next
           i 0]
      (if (= i rounds)
        cup->next
        (let [[a b c] (iterate cup->next (cup->next current))
              target (first (remove #{a b c} (possible-targets current)))
              new-cup->next (assoc cup->next
                              target a
                              c (cup->next target)
                              current (cup->next c))]
          (recur (new-cup->next current) new-cup->next (inc i)))))))


(defn part-1
  [input]
  (let [cup->next (play (read-cups input) 100)]
    (string/join "" (take (dec (count cup->next)) (iterate cup->next (cup->next 1))))))


(defn part-2
  [input]
  (let [base-cups (read-cups input)
        cups (take 1000000 (concat base-cups (drop (inc (count base-cups)) (range))))
        cup->next (play cups 10000000)]
    (* (cup->next 1) (cup->next (cup->next 1)))))


(def small-input
  "389125467")


(def large-input
  "643719258")


(defn play-fast
  "Just like `play`, but much uglier and 10+ times faster. ~7 seconds for part 2.
  `cups` must be a vector"
  [cups ^long rounds]
  (let [cup->next (long-array (inc (count cups)))
        _ (doseq [[^long k ^long v] (into [[(peek cups) (first cups)]] (partition 2 1 cups))]
            (aset cup->next k v))
        highest (long (count cups))
        possible-targets (object-array (inc (count cups)))
        _ (doseq [cup cups]
            (aset possible-targets cup (long-array (map (fn [^long x]
                                                          (if (< x 1)
                                                            (mod (unchecked-dec x) (unchecked-inc highest)) x))
                                                        (range (unchecked-dec cup) (- cup 5) -1)))))]
    (loop [^long current (first cups)
           i 0]
      (if (= i rounds)
        cup->next
        (let [a (aget cup->next current)
              b (aget cup->next a)
              c (aget cup->next b)
              target (long (first (remove (fn [^long x] (some (fn [^long y] (= x y)) [a b c]))
                                          ^longs (aget possible-targets current))))]
          (aset cup->next current (aget cup->next c))
          (aset cup->next c (aget cup->next target))
          (aset cup->next target a)
          (recur (aget cup->next current) (unchecked-inc i)))))))


(defn part-1-fast
  [input]
  (let [^longs cup->next (play-fast (read-cups input) 100)]
    (string/join "" (take (dec (dec (count cup->next))) (iterate #(aget cup->next %) (aget cup->next 1))))))


(defn part-2-fast
  [input]
  (let [base-cups (read-cups input)
        cups (into base-cups (range (inc (count base-cups)) 1000001))
        ^longs cup->next (play-fast cups 10000000)]
    (* (aget cup->next 1) (aget cup->next (aget cup->next 1)))))
