(ns advent-2022.day-11
  (:require [clojure.string :as string]))

(defn parse-input
  [input-string]
  (mapv (fn [monkey-string]
          (let [[monkey starting op-str test-str true-str false-str] (string/split-lines monkey-string)]
            {:monkey          (bigint (Integer/parseInt (str (get monkey 7))))
             :items           (mapv #(Integer/parseInt %)
                                    (string/split (last (string/split starting #": ")) #", "))
             :op              (let [[a op b] (string/split (last (string/split op-str #"new = ")) #" ")]
                                (fn [old]
                                  ((if (= op "*") * +)
                                   (if (= "old" a) old (bigint (Integer/parseInt a)))
                                   (if (= "old" b) old (bigint (Integer/parseInt b))))))
             :test            (bigint (Integer/parseInt (last (string/split test-str #" by "))))
             :true-recipient  (bigint (Integer/parseInt (last (string/split true-str #" monkey "))))
             :false-recipient (bigint (Integer/parseInt (last (string/split false-str #" monkey "))))}))
        (string/split input-string #"\n\n")))

(defn play-round-part-one
  [puzzle]
  (loop [puzzle puzzle
         monkey-idx 0]
    (let [monkey (get puzzle monkey-idx)]
      (cond
        (not monkey) puzzle
        (empty? (:items monkey)) (recur puzzle (inc monkey-idx))
        :else (let [item-worry (first (:items monkey))
                    new-worry (int (/ ((:op monkey) item-worry) 3))
                    test-result (zero? (rem new-worry (:test monkey)))
                    new-monkey-idx (if test-result (:true-recipient monkey) (:false-recipient monkey))
                    new-puzzle (-> puzzle
                                   (assoc-in [monkey-idx :items] (vec (rest (:items monkey))))
                                   (assoc-in [new-monkey-idx :items] (conj (:items (get puzzle new-monkey-idx)) new-worry)))]
                (print monkey-idx " ")
                (recur new-puzzle monkey-idx))))))

(defn part-one
  [input-string rounds]
  (let [puzzle (parse-input input-string)]
    (loop [puzzle puzzle
           counter 0
           items-history [(map :items puzzle)]]
      (if (= rounds counter)
        (let [counts (mapv (partial mapv count) (butlast items-history))]
          (mapv (fn [i]
                  (reduce + (mapv #(get % i) counts)))
                (range (count (first counts)))))
        (let [new-puzzle (play-round-part-one puzzle)]
          (recur new-puzzle (inc counter) (conj items-history (map :items new-puzzle))))))))

(defn play-round-part-two
  [puzzle worry-cap]
  (loop [puzzle puzzle
         monkey-idx 0
         inspections []]
    (let [monkey (get puzzle monkey-idx)]
      (cond
        (not monkey) [puzzle inspections]
        (empty? (:items monkey)) (recur puzzle (inc monkey-idx) inspections)
        :else (let [item-worry (first (:items monkey))
                    new-worry (mod ((:op monkey) item-worry) worry-cap)
                    test-result (zero? (rem new-worry (:test monkey)))
                    new-monkey-idx (if test-result (:true-recipient monkey) (:false-recipient monkey))
                    new-puzzle (-> puzzle
                                   (assoc-in [monkey-idx :items] (vec (rest (:items monkey))))
                                   (assoc-in [new-monkey-idx :items] (conj (:items (get puzzle new-monkey-idx)) new-worry)))]
                (recur new-puzzle monkey-idx (conj inspections monkey-idx)))))))

(defn part-two
  [input-string rounds]
  (let [puzzle (parse-input input-string)
        worry-cap (reduce * (map :test puzzle))]
    (loop [puzzle puzzle
           counter 0
           items-history [(map :items puzzle)]
           all-inspections []]
      (if (= rounds counter)
        (apply * (take-last 2 (sort (vals (frequencies all-inspections)))))
        (let [[new-puzzle inspections] (play-round-part-two puzzle worry-cap)]
          (recur new-puzzle (inc counter) (conj items-history (map :items new-puzzle)) (into all-inspections inspections)))))))

(def small-input "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1")

(def large-input "Monkey 0:\n  Starting items: 89, 73, 66, 57, 64, 80\n  Operation: new = old * 3\n  Test: divisible by 13\n    If true: throw to monkey 6\n    If false: throw to monkey 2\n\nMonkey 1:\n  Starting items: 83, 78, 81, 55, 81, 59, 69\n  Operation: new = old + 1\n  Test: divisible by 3\n    If true: throw to monkey 7\n    If false: throw to monkey 4\n\nMonkey 2:\n  Starting items: 76, 91, 58, 85\n  Operation: new = old * 13\n  Test: divisible by 7\n    If true: throw to monkey 1\n    If false: throw to monkey 4\n\nMonkey 3:\n  Starting items: 71, 72, 74, 76, 68\n  Operation: new = old * old\n  Test: divisible by 2\n    If true: throw to monkey 6\n    If false: throw to monkey 0\n\nMonkey 4:\n  Starting items: 98, 85, 84\n  Operation: new = old + 7\n  Test: divisible by 19\n    If true: throw to monkey 5\n    If false: throw to monkey 7\n\nMonkey 5:\n  Starting items: 78\n  Operation: new = old + 8\n  Test: divisible by 5\n    If true: throw to monkey 3\n    If false: throw to monkey 0\n\nMonkey 6:\n  Starting items: 86, 70, 60, 88, 88, 78, 74, 83\n  Operation: new = old + 4\n  Test: divisible by 11\n    If true: throw to monkey 1\n    If false: throw to monkey 2\n\nMonkey 7:\n  Starting items: 81, 58\n  Operation: new = old + 5\n  Test: divisible by 17\n    If true: throw to monkey 3\n    If false: throw to monkey 5")
