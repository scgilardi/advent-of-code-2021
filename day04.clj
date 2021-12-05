(ns day04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-int
  [s]
  (Integer/parseInt s))

(defn read-inputs
  "returns a vector of calls and a vector of grids, each a vector of 5 rows"
  [filename]
  (let [[line & lines] (-> (io/file filename)
                           io/reader
                           line-seq)
        calls (into [] (map parse-int) (str/split line #","))
        parse-row (fn [line]
                    (into [] (map parse-int) (str/split line #"\s+")))
        row-xd (comp (map str/trim)
                     (filter seq)
                     (map parse-row)
                     (mapcat seq)
                     (map #(vector % false))
                     (partition-all 25))
        boards (into [] row-xd lines)]
    [boards calls]))

(defn mark-cell
  [[val marked] call]
  (if (= val call)
    [val true]
    [val marked]))

(defn mark-board
  "board is a vector of 25 pairs"
  [board call]
  (into [] (map #(mark-cell % call)) board))

(defn mark-boards
  [boards call]
  (into [] (map #(mark-board % call)) boards))

(defn row-winner?
  [rows]
  (reduce (fn [acc row]
            (if (some false? (map second row))
              acc
              (reduced true)))
          false
          rows))

(defn transpose
  [m]
  (apply mapv vector m))

(defn winner?
  [board]
  (or (row-winner? (partition-all 5 board))
      (row-winner? (transpose (partition-all 5 board)))))

(defn score-board
  [call board]
  (* call (reduce (fn [acc [val marked]]
                    (if marked
                      acc
                      (+ acc val)))
                  0
                  board)))

(defn run-call
  [[scores active] call]
  (reduce (fn [[scores active] board]
            (if (winner? board)
              [(conj scores (score-board call board)) active]
              [scores (conj active board)]))
          [scores []]
          (mark-boards active call)))

(defn run-game
  [[boards calls]]
  (reduce run-call [[] boards] calls))

(defn day04-1
  [input]
  (let [[scores active] (run-game input)]
    (first scores)))

(defn day04-2
  [input]
  (let [[scores active] (run-game input)]
    (last scores)))

(comment
  (day04-1 (read-inputs "day04-test.txt"))
  (day04-2 (read-inputs "day04-test.txt"))
  (day04-1 (read-inputs "day04-input.txt"))
  (day04-2 (read-inputs "day04-input.txt"))
  )
