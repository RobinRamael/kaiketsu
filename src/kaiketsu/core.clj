(ns sudoku.core
  (:use sudoku.grid sudoku.util clojure.set))

;; get the indexes of the values that are not filled in yet in this col (nil)
(defn missing-idxs [coll]
  (remove nil?
	  (map-indexed
	   (fn [idx val] (when (not val) idx))
	   coll)))

;; get the values that are missing in this coll
;; coll = (range 9) gives an empty seq.
(defn missing [coll]
  (seq (difference (set (range 1 10)) coll)))

;; get the coords that are not filled in yet in
;; the box specified by the top right row and col.
(defn empty-coords-in-box [coll br bc]
  (map
   #(let [coord (grid-coord 9 %)]
      [(+ (coord 0) br) (+ (coord 1) bc)])
   (missing-idxs coll)))


;; get the values that are not candidates
;; for the value of (r, c)
(defn impossibles [g r c]
  (remove nil? (distinct (flatten (list (nth-col g c)
			    (nth-row g r)
			    (get-box g 9 r c))))))

;; try to find a value to fill in in this row and fill it in
(defn solve-row [g r]
  (let [ms (missing (nth-row g r)) ;; the values that are still to be filled in in this row
	midxs (missing-idxs (nth-row g r)) ;; the indexes of the empty places
	s (map  #(missing (impossibles g r %)) midxs)  ;; the possibilities for each of the empty places
	value (some #(when (= 1 (count-having s  %)) %) ms) ;; find a value that is only possible in one of the empty places
	c ((zipmap (map #(has? % value) s) midxs) true)] ;; find the coordinate of that empty place
    (if value ;; if the value was found
      (grid-assoc g r c value) ;; fill it in.
      g))) ;; if nothing was found, return the grid like it was


;; try to find a value to fill in in this col and fill it in
;; works the same as solve-col
(defn solve-col [g c]
  (let [ms (missing (nth-col g c))
	midxs (missing-idxs (nth-col g c))
	s (map  #(missing (impossibles g % c)) midxs) 
	value (some #(when (= 1 (count-having s %)) %) ms)
	r ((zipmap (map #(has? % value) s) midxs) true)]
    (if value
      (grid-assoc g r c value)
      g)))


;; try to find a value to fill in in the nth box  and fill it in
;; works roughly the same as solve-row, but works with coordinates in stead of indexes.
(defn solve-box [g n]
  (let [br (* (quot n 3) 3)   
	bc (* (mod n 3) 3)
	box (get-box g 9 br bc)
	missing-in-box (missing box)
	mcs (empty-coords-in-box box br bc)
	missing-per-coord (map (fn [coord]
				  (missing (impossibles g (coord 0) (coord 1))))
			       mcs)
	value (some
	       #(when (= 1 (count-having missing-per-coord %)) %)
	       missing-in-box)
	
	coord ((zipmap (map #(has? % 1) missing-per-coord) mcs) true)]
    (if (and value coord)
      (apply #(grid-assoc g %1 %2 value) coord)
      g)))


;; TODO: the solve-* funtions (especially row and col) should be easy to merge into one function which is given
;; just a seq of coordinates and the grid.



;; go over each of the rows once, cols and boxes and try to fill in one value.
(defn solve-grid-one-pass [g]
  (let [r (range 9)]
    (reduce solve-box
	    (reduce solve-row
		    (reduce solve-col g
			    r)
		    r)
	    r)))

;; solve the sudoku as far as possible.
;; when nothing has changed after another pass, return the grid.
(defn solve-grid [grid]
  (loop [g grid last-g nil]
    (if (and (has? g nil) (not= last-g g))
      (recur (solve-grid-one-pass g) g)
      g)))