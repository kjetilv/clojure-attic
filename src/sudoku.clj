(ns sudoku
  (:require [clojure.string :as str])
  (:require [clojure.math.numeric-tower :as math]))

(defn print-sud [sud & sep]
  (letfn [(to-x [x]
            (if (pos? x) x (if (nil? sep) " " (first sep))))
          (split [coll x sep]
            (if (empty? (drop x coll)) coll
              (concat (take x coll) (list sep) (split (drop x coll) x sep))))]
    (let [printable-rows (split (:rows sud) 3 "------|-------|------")]
      (letfn [(print-line [row]
                (or
                  (and (string? row) row)
                  (str/join " " (split (map to-x row) 3 "|"))))]
        (str/join "\n" (map print-line printable-rows))))))

(def base-coords
  (memoize (fn [i j]
             (let [x (dec i)
                   y (dec j)
                   box-x (int (/ x 3))
                   box-y (int (/ y 3))
                   box-index (+ (* (mod y 3) 3) (mod x 3))
                   box-no (+ (* 3 box-y) box-x)]
               { :x x :y y
                 :box-x box-x :box-y box-y
                 :box-no box-no
                 :box-index box-index }))))

(defn coords [sud [i j]]
  (let [{ x :x y :y
          box-x :box-x box-y :box-y
          box-no :box-no
          box-index :box-index } (base-coords i j)
        col (nth (:cols sud) x)
        row (nth (:rows sud) y)
        box (nth (:boxs sud) box-no)]
    { :coord [i j]
      :x x :y y
      :value (nth row x)
      :row row :col col :box box
      :box-x box-x :box-y box-y :box-no box-no
      :box-index box-index }))

(defn available [sud [i j]]
  (let [{ x :x row :row col :col box :box value :value} (coords sud [i j])]
    (if (pos? value)
      nil
      (apply vector
        (let [nums (set (concat row col box))]
          (remove nums (range 10)))))))

(defn free-spots [sud]
  (letfn [(completed? [{free :free-values}]
            (nil? free))
          (unsolvable? [{free :free-values}]
            (empty? free))
          (free-values-count
            [{free :free-values}] (count free))
          (free-spot [coord]
            { :coord coord :free-values (available sud coord)})
          (remaining-free-spots [free-coords]
            (for [coord free-coords]
              (free-spot coord)))
          (initial-free-spots []
            (remove completed? (for [y (range 1 10)
                                     x (range 1 10)]
                                 (free-spot [x y]))))]
    (let [free-coords (:free sud)
          free-spots (if free-coords
        (remaining-free-spots free-coords)
        (initial-free-spots))]
      (if (some unsolvable? free-spots)
        []
        (sort-by free-values-count free-spots)))))

(defn read-sud [sudoku-string]
  (let [numbers (set (seq (clojure.string/join "" (range 10))))]
    (letfn [(is-number? [^Character ch]
              (numbers ch))
            (non-number? [^Character ch]
              (not (is-number? ch)))
            (no-numbers? [line]
              (not (some is-number? (seq line))))
            (vectors [lists]
              (map #(apply vector %1) lists))
            (divide-into [size coll]
              (if (empty? coll) ()
                (cons (take size coll) (divide-into size (drop size coll)))))
            (non-number-remover [line]
              (remove non-number? line))

            (to-nums [line]
              (letfn [(to-num [x] (. Integer parseInt (str x)))]
                (map to-num line)))
            (to-columns [rows]
              (if (some empty? rows) ()
                (cons (map first rows) (to-columns (map rest rows)))))
            (to-boxes [rows]
              (letfn [(into-3 [row] (divide-into 3 row))
                      (collapse [list-of-lists] (reduce concat list-of-lists))]
                (collapse (map #(map collapse %1) (map to-columns (divide-into 3 (map into-3 rows)))))))]
      (let [lines (remove no-numbers? (map seq (str/split-lines sudoku-string)))
            rows (vectors (map to-nums (map non-number-remover lines)))
            cols (vectors (to-columns rows))
            boxs (vectors (to-boxes rows))
            sud { :rows (apply vector rows)
                  :cols (apply vector cols)
                  :boxs (apply vector boxs) }
            free (map :coord (free-spots sud))]
        (assoc sud :free free)))))

(defn solved? [sud]
  (letfn [(contains-zero? [row]
            (some zero? row))]
    (and
      (not (nil? sud))
      (not (some contains-zero? (:rows sud))))))

(defn branch-sud [sud [i j] v]
  (let [{ x :x y :y box-no :box-no box-index :box-index row :row col :col box :box } (coords sud [i j])
        changed-row (assoc row x v)
        changed-col (assoc col y v)
        changed-box (assoc box box-index v)]
    { :rows (assoc (:rows sud) y changed-row)
      :cols (assoc (:cols sud) x changed-col)
      :boxs (assoc (:boxs sud) box-no changed-box)
      :free (remove #(= %1 [i j]) (:free sud)) }))

(defn solve [sud]
  (letfn [(solve-int [sud depth]
            (if (solved? sud)
              { :solution sud
                :depth depth }
              (first (let [{ coord :coord free-values :free-values } (first (free-spots sud))]
                       (if (empty? free-values)
                         nil
                         (let [branch-candidate (fn [free-value] (branch-sud sud coord free-value))
                               solve-deeper (fn [candidate] (solve-int candidate (inc depth)))
                               candidates (map branch-candidate free-values)]
                           (remove nil? (map solve-deeper candidates))))))))]
    (solve-int sud 0)))

(defn -main []

  ; Warm-up: Evil sudoku a few times
  (let [sud (read-sud "306 200 000
                       000 603 000
                       090 080 403
                     
                       078 005 000
                       020 000 050
                       000 400 870
                     
                       504 070 090
                       000 301 000
                       000 002 105")]
    (dotimes [i 25] (solve sud)))

  ; The hardest sudokus start with 17 numbers. This is the minimum required for having a unique solution.  
  ; This sudoku starts with 16 numbers and has two solutions.  We find one, so far.
  (let [sud (read-sud "100 000 005
                       000 030 000
                       002 040 000

                       000 000 000
                       034 000 700
                       000 206 001

                       200 005 000
                       070 000 030
                       000 001 000")]
    (println (print-sud sud))
    (println)
    (let [solved-sud (time (solve sud))]
      (println (print-sud (:solution solved-sud)) "depth" (:depth solved-sud)))))

(-main)
