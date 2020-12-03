(ns advent-2020.day-1)


(defn part-one
  [input]
  (->> (for [i (range (count input))
             j (range (inc i) (count input))]
         [(get input i) (get input j)])
       (keep (fn [[a b]] (when (= 2020 (+ a b)) (* a b))))
       (first)))


(defn part-two
  [input]
  (->> (for [i (range (count input))
             j (range (inc i) (count input))
             k (range (inc j) (count input))]
         [(get input i) (get input j) (get input k)])
       (keep (fn [[a b c]] (when (= 2020 (+ a b c)) (* a b c))))
       (first)))
