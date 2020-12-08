(ns advent-2020.day-8
  (:require [clojure.string :as string]))


(defn decode-instruction
  [instruction-string]
  (let [[instruction arg] (string/split instruction-string #"\s+")]
    [instruction (Integer/parseInt arg)]))


(defn decode-instructions
  [input]
  (mapv decode-instruction (string/split-lines input)))


(defn part-1
  [input]
  (let [instructions (decode-instructions input)]
    (loop [i 0
           acc 0
           seen #{}]
      (if (seen i)
        acc
        (let [[instruction ^int arg] (get instructions i)]
          (case instruction
            "nop"
            (recur (inc i) acc (conj seen i))

            "acc"
            (recur (inc i) (+ acc arg) (conj seen i))

            "jmp"
            (recur (+ i arg) acc (conj seen i))))))
    ))


(defn part-2
  [input]
  (let [instructions (decode-instructions input)]
    (->> (range (count instructions))
         (keep
           (fn [change-idx]
             (let [[instruction] (get instructions change-idx)]
               (when (#{"nop" "jmp"} instruction)
                 (let [changed-instructions (update instructions change-idx
                                                    (fn [[instruction arg]]
                                                      [(if (= "nop" instruction) "jmp" "nop") arg]))]
                   (loop [i 0
                          acc 0
                          seen #{}]
                     (cond
                       (seen i)
                       nil

                       (= i (count changed-instructions))
                       acc

                       :else
                       (let [[instruction ^int arg] (get changed-instructions i)]
                         (case instruction
                           "nop"
                           (recur (inc i) acc (conj seen i))

                           "acc"
                           (recur (inc i) (+ acc arg) (conj seen i))

                           "jmp"
                           (recur (+ i arg) acc (conj seen i))))))))))
           )
         (first))
    ))


(def small-input
  "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6")


(def large-input
  "jmp +236\nacc +43\nacc +28\njmp +149\nacc +28\nacc +13\nacc +36\nacc +42\njmp +439\nacc -14\njmp +29\njmp +154\nacc +16\nacc -13\nacc -16\nnop +317\njmp +497\nacc +21\njmp +386\njmp +373\nacc +22\njmp +311\nacc -16\nacc +27\nacc +21\nacc +43\njmp +512\njmp +218\njmp +217\nacc +12\nacc +44\nnop +367\nnop +180\njmp +134\nacc -2\nacc +42\nacc +13\nacc -11\njmp +442\nnop +457\njmp +151\nacc +15\nacc -4\nacc +0\njmp +131\nacc +6\nacc -2\nacc +37\njmp +112\nacc +32\nacc +6\nacc -15\njmp +474\njmp +515\nacc +12\nacc +11\nacc +4\njmp +339\nacc -3\nacc +36\njmp +220\nnop +91\nacc -12\njmp +49\nacc -17\njmp +204\nacc +40\njmp +535\nacc +37\nacc +8\nnop +147\nnop +174\njmp +306\njmp +305\nacc +7\nacc +33\njmp +305\nacc +22\nacc +17\nacc +24\njmp +458\njmp +1\nacc +36\nacc +34\njmp +113\nacc -3\nnop +113\nnop -34\njmp +506\nacc -19\nacc +21\nacc +35\nacc -1\njmp +74\nacc +15\nacc +7\njmp +79\nacc +29\nacc +42\njmp +427\nacc +33\njmp +29\nacc +6\nacc +13\nnop +477\nacc +26\njmp +493\nacc +33\nacc +43\nacc +49\nacc +35\njmp +409\nacc -7\nacc +35\nacc +40\njmp +309\nacc -13\nacc -14\nacc +32\njmp +322\njmp +10\njmp +44\nacc +20\nacc +25\njmp +175\nacc +22\nacc +16\nacc +1\nacc +36\njmp -65\njmp +231\nacc +35\njmp +155\njmp +218\nacc -10\nacc -13\nacc +38\njmp -92\nacc +15\njmp +134\nacc -16\nacc +18\njmp -30\nnop -41\nacc +48\nacc +49\njmp -107\nacc +4\nacc +34\nacc +38\nacc -18\njmp +247\nacc +45\nacc +23\njmp +149\nnop +164\nacc +26\njmp -24\njmp +240\njmp +77\nacc +30\nacc -13\njmp -158\nnop +136\njmp +33\njmp +189\njmp +143\njmp +1\nacc +4\nacc +30\njmp -106\nacc +16\nnop -52\nacc +37\njmp +119\nacc -11\nacc -9\nacc +15\nacc +4\njmp +301\njmp +1\nacc -3\njmp +188\nnop +86\nnop +125\nacc -10\njmp -105\nacc +36\nacc +9\nacc +0\njmp +317\njmp +347\nacc +48\nnop +380\nacc -18\nacc +28\njmp +398\njmp -152\njmp -86\nacc +22\nacc +11\nacc +39\njmp -173\njmp +343\nnop +194\nnop +98\nnop +382\njmp +300\nacc +35\nnop +287\nacc -8\njmp +302\nacc +19\nacc +45\njmp +95\nacc +29\njmp +274\nacc +18\nacc -13\nacc +23\nacc +7\njmp +164\nacc +17\nacc +36\nacc -5\njmp +153\nacc +21\njmp +105\njmp +1\nnop +267\njmp +277\njmp +88\nacc +2\nacc +18\nnop +182\njmp +189\nacc +37\nacc +46\njmp +258\nacc +22\nacc +15\njmp +249\nacc +17\njmp -162\njmp +25\nacc -6\nnop +314\njmp -30\njmp +312\nacc +34\nnop -230\nacc -2\njmp +158\nacc -4\nacc +37\njmp +318\nacc +18\nacc +23\nacc -8\njmp -248\njmp +181\nacc +17\nacc +4\njmp -189\nacc +27\nacc -13\nacc -4\nacc +8\njmp +222\njmp +310\nacc -5\nacc +35\njmp +241\njmp -130\njmp +124\nacc -19\njmp +331\nacc -8\nacc +45\njmp +106\nacc +23\nacc +48\njmp -107\nacc +7\nacc -19\nacc +3\njmp +130\njmp -104\nnop +5\nacc +29\nacc +8\nacc -6\njmp +7\nacc +12\njmp +102\nacc -4\nacc +46\nacc -17\njmp -209\nacc +20\njmp -271\nacc +48\njmp +30\nnop +204\nacc -19\nacc +4\nacc +38\njmp +17\njmp +116\nacc -17\nacc +23\njmp -75\njmp -129\njmp +152\nacc +36\nnop -193\nacc +26\nacc +38\njmp +242\njmp -197\nacc +32\nacc -5\nacc -19\njmp -201\njmp -304\nacc +9\njmp +175\nacc +1\njmp -15\njmp +1\nnop -74\njmp -38\nnop -165\nacc -19\njmp -317\nacc -19\nacc -1\njmp +17\nacc +0\nnop +151\njmp +93\nacc +32\nacc +29\nacc +0\njmp -340\nacc +39\njmp -115\nacc +0\nacc +47\nnop -320\njmp +244\nacc +29\njmp +81\njmp -84\nacc +2\nacc +16\nnop -345\nacc +23\njmp +9\nacc +26\njmp -67\nacc -11\nacc +38\njmp +150\nacc +19\nacc -2\njmp -244\njmp +88\nacc -4\njmp -157\nacc +22\nacc +33\nacc +41\njmp -117\nacc +31\nacc +50\nacc +24\njmp -265\njmp +1\njmp -352\njmp -312\nacc +35\nacc +30\njmp -90\njmp +8\nacc +14\nacc +39\njmp -112\nacc -11\nacc -3\nacc +22\njmp -116\nacc +48\njmp -194\nacc -5\njmp -252\njmp +66\njmp -295\njmp +196\nacc +25\nacc -11\nnop +112\nacc +33\njmp +123\nacc -10\nacc +28\nnop -119\nacc +12\njmp -166\njmp -356\nacc +8\nacc +16\njmp +161\nacc +25\nacc +3\njmp -5\nacc +32\nacc +40\njmp +181\nacc -11\nacc -5\njmp +1\nacc +0\njmp -265\nacc +5\nacc +24\nacc +15\nacc -17\njmp -326\nnop +103\nacc -9\nacc +13\njmp -379\nacc +38\nacc +16\njmp -65\njmp +1\njmp +1\njmp +1\nacc -1\njmp -191\nacc +35\nacc -19\nacc -6\njmp -52\nacc +15\njmp -357\nnop -134\nacc -3\nnop +103\njmp -123\nacc +43\nacc +0\nacc +47\njmp -373\nacc +0\nacc +50\nacc +44\nacc +21\njmp -114\nacc -19\njmp -339\nacc +25\njmp -410\njmp -126\nacc -2\nacc -6\nacc +14\njmp -207\nacc +35\nacc -7\njmp +75\nacc +9\nacc +22\njmp +114\nacc +18\nacc +36\nacc +0\nacc +40\njmp -192\nacc +35\nacc +0\nacc +28\nacc +3\njmp -346\nnop -131\nacc +46\nnop -467\nnop -179\njmp -151\njmp -120\nacc +30\nacc +22\nacc -7\nacc +18\njmp -157\nacc +5\njmp +76\nnop -315\nacc +25\njmp -357\nacc +44\njmp -12\nacc +0\nacc +19\nnop -485\njmp -495\nnop -115\nacc +12\njmp -8\nacc +31\nacc -7\njmp -158\nacc +44\nacc +32\njmp +87\nacc +1\nacc +37\nacc +44\njmp -86\nacc +0\nacc +17\nacc -13\njmp -434\nacc +37\njmp -342\nacc +3\njmp +1\nacc +29\njmp -242\nacc +48\njmp -442\njmp -283\nacc -19\nacc +6\nacc +20\nacc +44\njmp -533\nacc -15\nnop -356\nacc +18\njmp -408\nacc -9\nacc +17\nacc +16\njmp -385\nnop -130\njmp +1\nacc +38\nacc +39\njmp -324\njmp -141\nacc +4\nacc +3\nacc -4\njmp -114\nacc +2\njmp +1\nacc +44\njmp -360\nacc +43\nacc +36\nnop -177\nnop -288\njmp -496\nacc +45\nacc +0\njmp -322\nacc +13\njmp -511\nacc -2\nacc +36\njmp -460\nacc +28\nacc +28\njmp -455\nacc -4\nacc +38\njmp -145\njmp -163\njmp -331\nnop -227\njmp -470\nacc +35\nnop -419\nacc +39\nacc +0\njmp -435\njmp +1\njmp -69\nacc +20\nacc +46\nnop +2\njmp -239\nacc -3\nacc +12\nacc +38\njmp -259\njmp -60\njmp -67\nnop -542\njmp -397\nacc +32\njmp -57\nacc +30\nnop -393\njmp -380\nacc +16\nacc -7\nacc +0\nacc +2\njmp +1")
