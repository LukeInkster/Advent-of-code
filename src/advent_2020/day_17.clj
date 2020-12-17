(ns advent-2020.day-17
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as string]))


(defn start-state
  [input dimensions]
  (let [slice (string/split-lines input)
        shift (/ (dec (count slice)) 2)
        active-points (->> slice
                           (map-indexed
                             (fn [y row]
                               (keep-indexed
                                 (fn [x v]
                                   (when (= \# v)
                                     (concat (repeat (- dimensions 2) 0)
                                             [(- y shift) (- x shift)])))
                                 row)))
                           (apply concat)
                           (set))]
    active-points))


(defn points-around
  [point]
  (let [diffs-list (remove (partial every? zero?) (combo/selections [-1 0 1] (count point)))]
    (map (fn [diffs] (map (fn [coord diff] (+ coord diff)) point diffs)) diffs-list)))


(defn update-active
  [active-points]
  (->>
    ;; Find all the unique points that are around an active point. These are
    ;; the the only points that could be active after this iteration
    (mapcat
      (fn [active-point]
        (conj (points-around active-point) active-point))
      active-points)
    (distinct)

    ;; Filter the points that could be active down to just the ones that satisfy the activity rules
    (filter
      (fn [point]
        (let [active-neighbour-count (->> (points-around point)
                                          (filter (fn [neighbour] (contains? active-points neighbour)))
                                          (count))]
          (or (and (contains? active-points point)
                   (<= 2 active-neighbour-count 3))
              (and (not (contains? active-points point))
                   (= 3 active-neighbour-count))))))
    (set)))


(defn iterate-times
  [active-points times]
  (nth (iterate update-active active-points) times))


(defn part-1
  [input]
  (count (iterate-times (start-state input 3) 6)))


(defn part-2
  [input]
  (count (iterate-times (start-state input 4) 6)))


(def small-input
  ".#.\n..#\n###")


(def large-input
  "##.#....\n...#...#\n.#.#.##.\n..#.#...\n.###....\n.##.#...\n#.##..##\n#.####..")
