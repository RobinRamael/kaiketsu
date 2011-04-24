(ns sudoku.grid
  (:use sudoku.util)
  (:require [clojure.string :as str]))

(defn make-grid [size]
  (vec (for [x (range (** size))] nil)))

(defn grid-size
  [grid]
  (sqrt (count grid)))

(defn grid-idx
  [grid r c]
  (+ c (* r (grid-size grid))))

(defn grid-coord
  [gridsize n]
  (vector (quot n (sqrt gridsize))
	  (mod n (sqrt gridsize))))

(defn grid-nth [grid r c]
  (grid (grid-idx grid r c)))

(defn grid-assoc [grid r c value]
  (assoc grid (grid-idx grid r c) value))

(defmacro get-line
  [grid f]
  `(map #(apply grid-nth ~grid %)
       (map ~f (range (grid-size ~grid)))))

(defn nth-col [grid c]
  (get-line grid #(vector % c)))

(defn nth-row [grid r]
  (get-line grid #(vector r %)))

(defn nth-row-without [grid r c]
  (remove-nth c (nth-row grid r)))

(defn nth-col-without [grid r c]
  (remove-nth r (nth-col grid c)))


(defn get-box [grid nr-boxes r c]
  (map #(apply grid-nth grid %)
       (let [gridsize (grid-size grid)
	     boxsize (/ gridsize (sqrt nr-boxes))
	     box-row  (* (quot r boxsize) boxsize)
	     box-col (* (quot c boxsize) boxsize)]
	 (map-indexed #(vector (+ (quot %1 boxsize) box-row) %2)
		      (flatten (repeat boxsize
				       (map
					#(+ %  box-col)
					(range boxsize))))))))

(defn nth-box [grid nr-boxes n]
  (let [r (sqrt nr-boxes)]
    (get-box grid nr-boxes
	     (* (quot n r) r)
	     (* (mod n r) r))))

(defn naive-read-grid [filename]
  (vec (map
	#(let [n (Integer/parseInt %)]
	   (when (< 0 n 10) n))
	(str/split (slurp filename) #"[ \n]+"))))

(defn valid? [grid]
  (= (** 9) (count grid)))

(defn read-grid [filename]
  (try (let [grid (naive-read-grid filename)]
	 (if (valid? grid)
	   grid
	   (throw (Exception. "Not a valid grid."))))
       (catch Exception e (str "Exception thrown while reading in grid: " (.getMessage e)))))


(defn hline
  ([length pattern]
     (str (apply str (repeat length pattern)) "\n" )))


(defn row-to-str [g rowidx]
 (str (apply str
	     (map
	      #(str "| " (if (nil? %) " " (str %)) " ")
	      (nth-row g rowidx)))
      "|\n"
      "|"
      (hline 3 (if (zero? (mod (inc rowidx) 3))
		 "--- --- ---|"
		 " -   -   - |"))))

(defn print-grid [g]
  (print (apply str
		(hline 9 " ---")
		(map
		 #(row-to-str g %)
		 (range 9)))))