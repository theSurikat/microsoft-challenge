(ns microsoft-challenge.core
  (:gen-class)
  (:require [clojure.string :as str])
  (:require [clojure.math.combinatorics :as combo])
  (:require [clojure.data.csv :as csv]))

(def outPut "output.txt")
(def big-string "Integer-set-product-combinations_InputForSubmission_2.txt")
(def practice "PracticeInput.txt")

(defn multi-tenth [x]
  (* x .1))

(defn multi-quarter [x]
  (* x .25))

(defn multi-half [x]
  (* x .5))

(def our-table
  {
    :O multi-tenth
    :L 100
    :M 100
    :F 100
    :T 100
    :S multi-half
    :B multi-half
    :W multi-half
    :C multi-quarter
    })

(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))

(defn file-to-list-string [file]
 "Takes a text file and turns it to a list of it's contents"
 (map parse-int
   (str/split (slurp file) #",")))

(defn find-prods [file]
    (drop 1
      (sort
        (map #(apply * %)
          (map #(into [] %)
            (combo/subsets (file-to-list file)))))))

(defn writeDaStuff [fileIn fileOut]
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
      (drop 1
          (file-to-list-string file)))))

(defn set-value [set idx]
  (let [cur (nth set idx)]
    (if (not (= cur "F"))
      (let [num-cottage (find-cottages set idx)])
      (let [open-land (find-all-land set idx)]))
    (cond
      (= cur "L") (assoc set idx
                    ["wood" (+ 100 (* 100 (+ (* num-cottage 0.25) (if (saw-mil?) 0.5 0))))])
      (= cur "M") (assoc set idx
                    ["ore" (+ 100 (* 100 (+ (* num-cottage 0.25) (if (black-smith?) 0.5 0))))])
      (= cur "F") (assoc set idx
                    ["food" (+ 100 (* 100 (+ (* open-land 0.1) (if (wind-mill?) 0.5 0))))])
      (= cur "T") (assoc set idx
                    ["gold" (+ 100 (* 100 (* num-cottage 0.25))) ]))))
