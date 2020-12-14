(ns advent-2020.day-12
  (:require [clojure.string :as string]))


(defn decode
  [input]
  (mapv (fn [command]
          [(first command) (Integer/parseInt (subs command 1))])
        (string/split-lines input)))


(defn new-n-e
  [command-direction units n e]
  (cond
    (= \N command-direction) [(+ n units) e]
    (= \S command-direction) [(- n units) e]
    (= \E command-direction) [n (+ e units)]
    (= \W command-direction) [n (- e units)]))


(defn new-direction
  [current-direction command-direction units]
  (let [dir->idx {\N 0 \E 1 \S 2 \W 3}
        idx->dir {0 \N 1 \E 2 \S 3 \W}
        curr-idx (dir->idx current-direction)
        new-idx (mod (if (= \L command-direction)
                       (- curr-idx (/ units 90))
                       (+ curr-idx (/ units 90)))
                     4)]
    (idx->dir new-idx)))


(defn part-1
  [input]
  (let [commands (decode input)]
    (loop [[[command-direction units] & more-commands] commands
           n 0
           e 0
           current-direction \E]
      (println n e current-direction [command-direction units])
      (cond
        (nil? command-direction)
        (+ (Math/abs (long n)) (Math/abs (long e)))

        (#{\N \S \E \W} command-direction)
        (let [[new-n new-e] (new-n-e command-direction units n e)]
          (recur more-commands new-n new-e current-direction))

        (= \F command-direction)
        (let [[new-n new-e] (new-n-e current-direction units n e)]
          (recur more-commands new-n new-e current-direction))

        (#{\L \R} command-direction)
        (let [new-direction (new-direction current-direction command-direction units)]
          (recur more-commands n e new-direction))))))


(defn rotate-way
  [command-direction units way-n way-e]
  (let [clockwise-steps (mod (if (= \R command-direction)
                               (/ units 90)
                               (- (/ units 90))) 4)]
    (cond
      (= 1 clockwise-steps) [(- way-e) way-n]
      (= 2 clockwise-steps) [(- way-n) (- way-e)]
      (= 3 clockwise-steps) [way-e (- way-n)]
      (= 4 clockwise-steps) [way-n way-e]))
  )

(defn part-2
  [input]
  (let [commands (decode input)]
    (loop [[[command-direction units] & more-commands] commands
           [n e] [0 0]
           [way-n way-e] [1 10]]
      (cond
        (nil? command-direction)
        (+ (Math/abs (long n)) (Math/abs (long e)))

        (#{\N \S \E \W} command-direction)
        (recur more-commands [n e] (new-n-e command-direction units way-n way-e))

        (= \F command-direction)
        (let [new-n (+ n (* units way-n))
              new-e (+ e (* units way-e))]
          (recur more-commands [new-n new-e] [way-n way-e]))

        (#{\L \R} command-direction)
        (recur more-commands [n e] (rotate-way command-direction units way-n way-e))))))


(def small-input
  "F10\nN3\nF7\nR90\nF11")


(def large-input
  "W2\nN4\nR90\nE3\nN2\nW4\nS5\nF83\nE5\nF53\nS3\nL90\nE1\nS2\nN2\nW5\nE4\nL180\nE4\nN1\nF27\nL90\nF9\nE3\nN2\nN3\nR90\nN5\nF57\nW5\nR180\nR180\nW5\nF44\nL90\nE5\nF87\nR180\nF61\nE4\nF37\nE2\nF39\nL180\nF53\nS1\nW1\nS2\nE2\nL90\nW4\nN5\nE1\nS1\nF31\nL90\nW5\nL180\nW1\nN5\nR90\nN5\nR90\nF94\nS5\nR90\nS2\nF94\nS3\nE1\nE5\nF9\nL90\nW5\nF83\nN2\nN5\nL90\nF33\nW4\nL90\nE5\nS5\nF23\nW5\nN1\nE3\nS1\nN1\nF59\nN1\nE1\nS2\nF56\nS2\nE5\nR180\nS4\nR180\nF46\nL90\nF78\nE5\nL180\nS4\nF22\nS5\nF32\nL90\nF68\nL90\nS3\nF76\nE3\nF71\nR90\nF34\nL90\nW5\nR90\nF12\nF65\nN4\nW5\nF65\nR270\nF13\nW2\nS2\nR90\nN1\nF14\nL180\nW4\nN5\nR180\nN2\nR90\nS3\nF1\nW2\nF8\nL90\nF98\nN5\nE3\nR90\nN3\nF39\nL180\nF87\nE3\nR180\nE4\nR90\nW4\nL180\nW2\nL90\nS1\nW2\nR180\nN3\nL90\nW4\nS4\nL90\nS4\nF75\nR90\nR180\nN4\nE5\nF9\nF40\nS3\nR90\nS2\nF26\nE2\nL180\nS4\nN5\nW1\nS5\nW3\nF11\nE2\nN5\nW3\nS5\nR90\nN2\nE4\nL90\nR90\nF8\nE4\nR90\nN2\nL90\nN3\nF8\nE2\nF67\nW5\nF19\nS3\nL90\nS3\nL90\nW1\nF54\nS1\nR90\nS4\nE1\nS3\nL90\nF14\nW4\nW3\nF36\nE5\nR90\nF10\nW2\nS1\nW2\nN5\nW4\nF64\nW5\nS4\nF13\nE5\nN1\nF87\nE3\nS4\nE5\nW3\nF46\nS5\nR270\nS4\nE3\nR90\nF97\nF92\nE2\nF17\nR90\nF5\nN1\nF89\nN5\nF55\nR90\nF51\nS3\nF97\nL90\nW5\nR90\nF7\nL180\nL180\nW5\nF88\nW2\nF26\nR180\nS4\nF54\nS1\nR90\nF66\nR90\nF6\nL90\nN5\nL90\nR90\nF58\nE3\nF67\nS1\nR90\nW4\nN4\nL90\nF63\nE3\nR90\nE4\nN4\nL180\nN3\nF34\nE5\nR90\nW1\nR90\nN3\nF73\nN5\nR90\nF28\nW1\nW3\nF38\nN3\nE1\nS5\nS2\nF72\nR90\nF25\nN3\nE2\nS3\nF63\nL270\nN3\nE5\nR90\nN4\nE3\nS1\nF32\nS5\nW3\nF98\nE2\nS5\nL90\nN5\nW4\nL90\nF68\nE2\nF81\nN2\nE4\nL90\nE1\nL90\nE1\nL180\nW3\nF99\nR90\nW1\nS4\nL90\nS4\nR90\nN2\nF17\nE3\nF78\nW1\nS2\nL180\nN5\nL90\nN2\nE4\nL90\nW1\nN2\nF97\nW3\nS5\nL180\nS4\nF77\nL90\nF55\nW3\nN4\nE4\nR90\nE5\nS3\nL90\nE1\nR90\nF54\nL90\nN5\nE4\nR90\nF41\nL90\nN1\nR90\nE5\nR180\nW2\nF74\nL90\nF88\nN3\nF25\nL180\nE2\nS1\nW4\nN1\nW5\nR180\nF31\nE1\nR180\nF17\nN1\nW2\nR180\nF61\nL270\nW4\nL180\nF66\nE4\nF68\nL90\nW4\nL180\nE4\nS1\nF30\nS3\nE1\nF93\nL90\nF33\nN3\nL90\nF58\nR90\nR90\nF23\nN5\nW2\nN3\nW4\nL180\nN1\nF84\nW5\nE5\nF36\nW3\nN3\nW3\nR180\nW2\nS3\nE4\nF62\nL90\nS2\nW4\nF28\nE1\nS5\nF54\nS5\nR270\nF35\nN4\nR90\nF38\nW4\nS3\nW2\nR90\nN2\nL270\nF21\nR90\nW5\nR180\nF7\nW1\nF72\nE3\nL180\nE1\nF42\nL270\nF1\nR90\nE4\nF72\nW3\nR90\nE4\nS4\nW4\nR90\nF98\nR90\nF100\nR90\nE1\nF9\nN1\nF81\nS5\nL90\nL90\nW3\nL90\nF75\nL90\nF27\nE3\nL90\nF49\nF53\nL90\nF26\nW1\nF48\nW1\nL90\nW1\nL90\nF71\nS1\nF34\nS1\nL90\nS2\nN3\nL180\nE1\nF52\nS5\nR90\nE4\nF58\nW2\nR90\nE5\nN3\nR180\nF56\nL90\nF92\nS1\nE2\nF68\nF24\nN3\nF29\nS4\nL90\nN5\nL90\nF48\nS5\nF80\nR90\nF34\nS5\nF23\nF36\nW2\nF57\nW5\nN1\nS2\nR90\nF94\nL90\nN2\nF95\nR180\nN1\nW1\nF59\nN5\nF62\nS4\nL90\nN4\nE2\nF55\nL90\nF21\nE2\nF52\nW2\nR90\nN3\nW5\nS1\nL90\nW1\nR90\nR90\nF21\nE4\nF47\nE5\nN5\nW3\nF34\nF2\nN1\nL90\nS3\nR90\nW1\nN4\nF49\nW1\nF15\nE5\nR90\nS4\nF39\nN4\nR90\nN4\nF69\nE2\nN5\nR90\nF21\nW5\nS5\nE4\nS3\nF67\nE3\nS2\nR90\nF51\nL90\nN5\nF73\nS1\nF18\nR180\nW2\nN1\nW5\nL90\nW2\nR90\nE2\nL90\nW3\nL90\nF13\nL90\nF45\nR90\nF85\nE2\nF44\nF65\nL90\nF82\nW2\nL270\nF65\nN3\nW3\nR90\nE3\nF20\nR90\nS2\nS3\nR180\nN4\nF98\nW5\nS2\nF63\nR90\nF88\nW3\nF1\nS4\nF39\nR180\nN3\nF84\nN4\nF51\nE1\nN5\nE3\nF70\nL90\nN3\nL180\nF63\nS2\nL90\nF16\nF11\nR180\nF70\nE2\nL90\nF46\nN2\nE1\nS1\nF19\nN5\nW1\nF67\nR90\nF79\nS2\nW5\nF96\nN1\nF53\nE3\nR90\nE1\nF78\nL90\nF61\nE5\nF85\nL90\nW4\nF72\nW1\nS5\nF49\nW1\nN1\nE2\nR90\nE2\nL90\nS5\nR90\nE2\nS4\nE3\nF8\nR90\nN3\nL90\nW1\nF56\nE1\nW4\nN5\nR90\nF47\nR90\nW1\nR90\nW5\nF5")
