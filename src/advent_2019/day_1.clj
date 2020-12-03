(ns advent-2019.day-1)


(defn fuel-for-mass
  [mass]
  (- (quot mass 3) 2))


(defn q1
  [masses]
  (reduce + (map fuel-for-mass masses)))


(defn module-fuel-q2
  [mass]
  (let [fuel (fuel-for-mass mass)]
    (if (pos? fuel)
      (let [fuel-for-the-fuel (module-fuel-q2 fuel)]
        (+ fuel fuel-for-the-fuel))
      0)))


(defn q2
  [masses]
  (reduce + (map module-fuel-q2 masses)))
