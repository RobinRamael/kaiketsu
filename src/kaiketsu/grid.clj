(ns kaiketsu.grid
  (:use kaiketsu.util)
  (:require [clojure.string :as str]))

;; make a square grid with the given length of sides.
(defn make-grid [size]
  (vec (for [x (range (** size))] nil)))

;; get the size of the sides of the given grid
(defn grid-size
  [grid]
  (sqrt (count grid)))

;; helperfunction for the nth-functions. Returns 
;; the index in the vector that represents a grid for
;; the given row and column.
(defn grid-idx
  [grid r c]
  (+ c (* r (grid-size grid))))

;; the opposite if grid-idx.  returns the coordinates
;; for the given index in the grid as a vector.
(defn grid-coord
  [gridsize n]
  (vector (quot n (sqrt gridsize))
	  (mod n (sqrt gridsize))))

;; get the element at row r and column c in the grid.
(defn grid-nth [grid r c]
  (grid (grid-idx grid r c)))


;; 'set' the element at row r and column c in the grid.
(defn grid-assoc [grid r c value]
  (assoc grid (grid-idx grid r c) value))

;;macro used by the nth functions.
(defmacro get-line
  [grid f]
  `(map #(apply grid-nth ~grid %) ;; extract the actual values
       (map ~f (range (grid-size ~grid))))) ;; get the coordinates

;; get all values in column c
(defn nth-col [grid c]
  (get-line grid #(vector % c)))

;; get all values in row r
(defn nth-row [grid r]
  (get-line grid #(vector r %)))

(defn nth-row-without [grid r c]
  (remove-nth c (nth-row grid r)))

(defn nth-col-without [grid r c]
  (remove-nth r (nth-col grid c)))

;; get the values of the box that contains the coordinate (r, c). A box is
;; an equal division of the grid and specified by nr-boxes.
;; in sudoku, this is 9.
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

;; get the nth box, counting from right to left, top to bottom.
(defn nth-box [grid nr-boxes n]
  (let [r (sqrt nr-boxes)]
    (get-box grid nr-boxes
	     (* (quot n r) r)
	     (* (mod n r) r))))


;; read and parse the grid. always use this wrapped in read-grid
(defn naive-read-grid [filename]
  (vec (map
	#(let [n (Integer/parseInt %)]
	   (when (< 0 n 10) n))
	(str/split (slurp filename) #"[ \n]+"))))

;; check if this grid is valid. TODO: this needs more checking.
(defn valid? [grid]
  (= (** 9) (count grid)))


;; read and parse the grid with the given filename.
(defn read-grid [filename]
  (try (let [grid (naive-read-grid filename)]
	 (if (valid? grid)
	   grid
	   (throw (Exception. "Not a valid grid."))))
       (catch Exception e (str "Exception thrown while reading in grid: " (.getMessage e)))))

;; helperfunction for print-grid. prints a horizontal line of the given pattern (n times)
(defn hline
  ([n pattern]
     (str (apply str (repeat n pattern)) "\n" )))

;; helperfunction for print-grid.
;; prettifies the  row in the grid with the given index.
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

;; pretty print the grid.
(defn print-grid [g]
  (print (apply str
		(hline 9 " ---")
		(map
		 #(row-to-str g %)
		 (range 9)))))