(ns day03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-inputs
  "returns a vector of seqs of binary digits"
  [filename]
  (let [lines (-> (io/file filename)
                  io/reader
                  line-seq)
        parse-line (fn [line]
                     (into [] (map (fn [c] (- (int c) 48))) line))]
    (into [] (map parse-line) lines)))

(defn most-common
  "returns a vector of most common digits in the column of inputs"
  [inputs prefer]
  (let [threshold (/ (count inputs) 2)
        counts (for [offset (range (count (first inputs)))]
                 (apply + (map #(nth % offset) inputs)))]
    (mapv (fn [x]
            (if (>= x threshold)
              prefer
              (- 1 prefer)))
          counts)))

(defn binary-to-int
  "big endian array of 1s and 0s to integer value"
  [bits]
  (reduce (fn [acc val]
            (+ (* 2 acc) val))
          0
          bits))

(defn decode
  [inputs prefer]
  (-> (most-common inputs prefer)
      binary-to-int))

(defn gamma
  [inputs]
  (decode inputs 1))

(defn epsilon
  [inputs]
  (decode inputs 0))

(defn day03-1
  [inputs]
  (* (gamma inputs) (epsilon inputs)))

;; ----------------------------------------------------------------------

(defn most-common-first
  "returns the most common of 1 or 0 in the first column of inputs"
  [inputs prefer]
  (if (>= (apply + (map first inputs)) (/ (count inputs) 2))
    prefer
    (- 1 prefer)))

(defn rating
  [inputs prefer]
  (loop [inputs inputs
         prefix []]
    (let [selector (most-common-first inputs prefer)
          filtered (filter #(= selector (first %)) inputs)]
      (if (= 1 (count filtered))
        (binary-to-int (into prefix (first filtered)))
        (recur (map rest filtered)
               (conj prefix selector))))))

(defn oxygen-generator-rating
  [inputs]
  (rating inputs 0))

(defn co2-scrubber-rating
  [inputs]
  (rating inputs 1))

(defn day03-2
  [inputs]
  (* (oxygen-generator-rating inputs)
     (co2-scrubber-rating inputs)))

(comment
  (day03-1 (read-inputs "day03-test.txt"))
  (day03-1 (read-inputs "day03-input.txt"))
  (day03-2 (read-inputs "day03-test.txt"))
  (day03-2 (read-inputs "day03-input.txt"))
)
