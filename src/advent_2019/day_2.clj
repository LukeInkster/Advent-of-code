(ns advent-2019.day-2)


(defn apply-binary
  [intcode i operator]
  (assoc intcode
    (get intcode (+ i 3))
    (operator
      (get intcode (get intcode (inc i)))
      (get intcode (get intcode (+ i 2))))))


(defn run
  [intcode]
  (loop [intcode intcode
         i 0]
    (case (get intcode i)
      1 (recur (apply-binary intcode i +) (+ i 4))
      2 (recur (apply-binary intcode i *) (+ i 4))
      99 (first intcode))))


(def q1 run)


(defn q2
  [intcode desired-result]
  (first (for [noun (range 0 100)
               verb (range 0 100)
               :when (= desired-result (run (assoc intcode 1 noun 2 verb)))]
           (+ (* 100 noun) verb))))
