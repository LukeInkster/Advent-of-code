(ns advent-2019.day-3
  (:require [clojure.string :as string]
            [clojure.set :as set]))


(defn parse-path
  [path-str]
  (map (fn [s]
         [(first s) (Integer/parseInt (apply str (rest s)))])
       (string/split path-str #",")))


(defn change-index-and-sign
  [direction]
  (case direction
    \U [1 +]
    \D [1 -]
    \L [0 -]
    \R [0 +]))


(defn coords-for-path
  [path]
  (loop [coords [[0 0]]
         [[direction distance] & remaining-path] (parse-path path)]
    (if direction
      (let [[change-index change-sign] (change-index-and-sign direction)]
        (recur (into coords (for [change-distance (range 1 (inc distance))]
                              (update (peek coords) change-index change-sign change-distance)))
               remaining-path))
      (rest coords))))


(defn closest-intersection-q1
  [path-1 path-2]
  (let [coords-1 (coords-for-path path-1)
        coords-2 (coords-for-path path-2)]

    (->> (set/intersection (set coords-1) (set coords-2))
         (map (fn [[^int x ^int y]] (+ (Math/abs x) (Math/abs y))))
         (apply min))
    ))


(defn closest-intersection-q2
  [path-1 path-2]
  (let [coords-1 (coords-for-path path-1)
        coords-2 (coords-for-path path-2)]

    (->> (set/intersection (set coords-1) (set coords-2))
         (map (fn [coords] (+ (inc (.indexOf coords-1 coords))
                              (inc (.indexOf coords-2 coords)))))
         (apply min))
    ))
