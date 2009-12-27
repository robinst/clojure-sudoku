(defn print-puzzle [puzzle]
  (doseq [line (partition 9 puzzle)]
    (println line)))

(defn subvec-length [v start length]
  (subvec v start (+ start length)))

(defn get-row-number [index]
  (quot index 9))

(defn get-col-number [index]
  (rem index 9))

(defn get-row [puzzle index]
  (let [row-number (get-row-number index)]
    (subvec-length puzzle (* row-number 9) 9)))

(defn get-col [puzzle index]
  (let [col-number (get-col-number index)]
    (take-nth 9 (drop col-number puzzle))))

(defn get-block-start [number]
  (* (quot number 3) 3))

(defn get-block [puzzle index]
  (let [start-row (get-block-start (get-row-number index))
        start-col (get-block-start (get-col-number index))]
    (mapcat
      (fn [row] (subvec-length puzzle (+ (* (+ start-row row) 9) start-col) 3))
      (range 3))))

(defn find-first-empty [puzzle]
  (some
   (fn [index] (if (zero? (puzzle index)) index))
   (range (count puzzle))))

(defn get-possible-numbers [puzzle index]
  (remove
    (set
      (concat
        (get-row puzzle index)
        (get-col puzzle index)
        (get-block puzzle index)))
    (range 1 10)))

(defn solve [puzzle]
  (let [index (find-first-empty puzzle)]
    (if index
      (let [possible-numbers (get-possible-numbers puzzle index)]
        (mapcat
          (fn [possible-number]
            (solve (assoc puzzle index possible-number)))
          possible-numbers))
      (list puzzle))))


(def puzzle
  [5 3 0  0 7 0  0 0 0
   6 0 0  1 9 5  0 0 0
   0 9 8  0 0 0  0 6 0
   8 0 0  0 6 0  0 0 3
   4 0 0  8 0 3  0 0 1
   7 0 0  0 2 0  0 0 6
   0 6 0  0 0 0  2 8 0
   0 0 0  4 1 9  0 0 5
   0 0 0  0 8 0  0 7 9])

(let [solutions (solve puzzle)]
  (println "Number of solutions: " (count solutions))
  (doseq [solution solutions]
    (print-puzzle solution)))
