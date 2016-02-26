(ns microsoft-challenge.core
  (:gen-class)
  (:require [clojure.string :as str]))

(def big-string "")

(defn file-to-list [file]
  "Takes a text file and turns it to a list of it's contents"
  (map #(str/replace % #"\"" "")
    (str/split (slurp file) #",")))
