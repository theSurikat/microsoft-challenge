(ns microsoft-challenge.core
  (:gen-class)
  (:require [clojure.string :as str])
  (:require [clojure.math.combinatorics :as combo])
  (:require [clojure.data.csv :as csv])
  (:require [clojure.set :as set]))

(def outPut "output.txt")
(def big-string "Integer-set-product-combinations_InputForSubmission_2.txt")
(def practice "PracticeInput.txt")

(defn parse-int [s]
  "Parse the file to seperate integers listed"
   (Integer. (re-find  #"\d+" s )))

(defn file-to-list [file]
 "Takes a text file and turns it to a list of it's contents"
 (map parse-int
   (str/split (slurp file) #",")))

(defn find-prods [file]
  "Finds the products of the list"
  (drop 1
    (sort
      (map #(apply * %)
        (map #(into [] %)
          (combo/subsets (file-to-list file)))))))

(defn writeDaStuff [fileIn fileOut]
  "writes the output to a file"
  (spit fileOut
    (apply str
      (interpose ","
        (map str
          (find-prods fileIn))))))


(defn file-to-list-string [file]
  "Takes a text file and turns it to a list of it's contents"
  (str/split (slurp file) #"\r\n"))

(defn string-list-to-vectors [file]
  "Turns a list of strings into vectors"
  (map #(into [] %)
    (map #(str/split % #"")
          (file-to-list-string file))))

(defn sum
  "Take the sum of a collection"
  ([xx] (sum xx 0))
  ([xx total]
    (if (empty? xx)
      total
      (recur (rest xx) (+ (first xx) total)))))

(defn count-goal [look-for set idx]
  "Counts the number of cottages in a set around the index"
    (sum
    [
      (if (= look-for (nth set (- idx 1) 0)) 1 0)
      (if (= look-for (nth set idx 0)) 1 0)
      (if (= look-for (nth set (+ idx 1) 0)) 1 0) ] ))

(defn find-all
  "Finds all the cottages near a square"
  [look-for set-above set set-below idx]
  (sum
    [
      (count-goal look-for set-above idx)
      (count-goal look-for set idx)
      (count-goal look-for set-below idx) ]))

(defn not-nil?
  "Sees if X is null"
  [x]
  (complement (nil? x)))

(defn set-value
  "Set the value of a sqaure"
  [set-above set set-below idx]
  (let [cur (nth set idx)]
  (let [num-cottage (find-all "C" set-above set set-below idx)]
  (let [open-land (find-all "O" set-above set set-below idx)]
    (cond
      (= cur "L") {:wood
                  (+ 100
                    (* 100
                      (+
                        (* num-cottage 0.25)
                        (if (zero? (find-all "S" set-above set set-below idx)) 0 0.5))))}
      (= cur "M") {:ore
                  (+ 100
                    (* 100
                      (+
                        (* num-cottage 0.25)
                        (if (zero? (find-all "B" set-above set set-below idx)) 0 0.5))))}
      (= cur "F") {:food
                  (+ 100
                    (* 100
                      (+
                        (* open-land 0.1)
                        (if (zero? (find-all "W" set-above set set-below idx)) 0 0.5))))}
      (= cur "T") {:gold
                  (+ 100
                    (* 100 (* num-cottage 0.25)))}
                    )))))

(defn set-all-values-in-set
  "Given the set, the set above, and below it, sets the value of all sets"
  [set-above set set-below range]
  (remove nil?
    (map #(set-value set-above set set-below %) range)))

(defn all-values-for-set
  "Given a set of vectors, return a set of maps"
  ([master-set width]
    (all-values-for-set
      (first master-set)
      (rest master-set)
      width
      (set-all-values-in-set
        nil
        (first master-set)
        (second master-set)
        (range width))))
  ([above-set master-set width return-set]
    (if (empty? master-set)
      (apply merge-with + return-set)
      (recur
        (first master-set)
        (rest master-set)
        width
        (set/union
          return-set
          (set-all-values-in-set
            above-set
            (first master-set)
            (second master-set)
            (range width)))))))

(defn map-returns
  "Given an input file, return the relevent information"
  [file]
  (all-values-for-set
    (drop 1 (string-list-to-vectors file))
    (Integer. (last (first (string-list-to-vectors file))))))

(defn output-of-game
  "Given an input game file, print the output of the game"
  [fileIn fileOut]
  (spit fileOut
    (apply str
      (interpose " "
        (map #(if (nil? %) 0 %)
          (map #(% (map-returns fileIn)) [:wood :ore :food :gold]))))))
