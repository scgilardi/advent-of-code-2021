(ns day02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-commands
  "returns a vector of pairs, each pair is like [:forward 30]"
  [filename]
  (let [lines (-> (io/file filename)
                  io/reader
                  line-seq)
        parse-line (fn [line]
                     (let [[dir param] (str/split line #" ")]
                       [(keyword dir) (Integer/parseInt param)]))]
    (into [] (map parse-line) lines)))

(defprotocol Rule
  "Encapsulates details of a rule for processing commands"
  (init [this])
  (reducer [this])
  (finalize [this acc]))

(defrecord Rule-1 []
  Rule
  (init [this] [0 0])
  (reducer [this]
    (fn [[x y] [dir param]]
      (case dir
        :forward [(+ x param) y]
        :up [x (- y param)]
        :down [x (+ y param)])))
  (finalize [this acc]
    (let [[x y] acc]
      (* x y))))

(defrecord Rule-2 []
  Rule
  (init [this] [0 0 0])
  (reducer [this]
    (fn [[x y aim] [dir param]]
      (case dir
        :forward [(+ x param) (+ y (* aim param)) aim]
        :up [x y (- aim param)]
        :down [x y (+ aim param)])))
  (finalize [this acc]
    (let [[x y aim] acc]
      (* x y))))

(defn run
  [rule commands]
  (finalize rule (reduce (reducer rule) (init rule) commands)))

(comment
  (run (->Rule-1) (read-commands "day02-test.txt"))
  (run (->Rule-2) (read-commands "day02-input.txt"))
  (run (->Rule-2) (read-commands "day02-test.txt"))
  (run (->Rule-2) (read-commands "day02-input.txt"))
  )
