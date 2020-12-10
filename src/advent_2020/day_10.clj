(ns advent-2020.day-10
  (:require [clojure.string :as string]))


(defn sorted-joltages
  [input]
  (let [joltages (map #(Integer/parseInt %) (string/split-lines input))]
    (vec (sort (conj joltages 0 (+ 3 (apply max joltages)))))))


(defn part-1
  [input]
  (let [joltages (sorted-joltages input)
        steps (frequencies (map - (rest joltages) (butlast joltages)))]
    (* (steps 1) (steps 3))))


(def count-paths
  (memoize
    (fn [joltages]
      (if (= 1 (count joltages))
        1
        (->> (range 1 (inc (count (take-while #(>= (+ (first joltages) 3) %) (rest joltages)))))
             (map (fn [step] (count-paths (drop step joltages))))
             (reduce +))))))


(defn part-2
  [input]
  (count-paths (sorted-joltages input)))


(def small-input
  "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4")


(def small-input-2
  "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3")


(def large-input
  "46\n63\n21\n115\n125\n35\n89\n17\n116\n90\n51\n66\n111\n142\n148\n60\n2\n50\n82\n20\n47\n24\n80\n101\n103\n16\n34\n72\n145\n141\n124\n14\n123\n27\n62\n61\n95\n138\n29\n7\n149\n147\n104\n152\n22\n81\n11\n96\n97\n30\n41\n98\n59\n45\n88\n37\n10\n114\n110\n4\n56\n122\n139\n117\n108\n91\n36\n146\n131\n109\n31\n75\n70\n140\n38\n121\n3\n28\n118\n54\n107\n84\n15\n76\n71\n102\n130\n132\n87\n55\n129\n83\n23\n42\n69\n1\n77\n135\n128\n94")
