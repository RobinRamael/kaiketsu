(ns sudoku.core
  (:use sudoku.grid sudoku.util clojure.set))

(defn missing-idxs [coll]
  (remove nil?
	  (map-indexed
	   (fn [idx val] (when (not val) idx))
	   coll)))

(defn missing [coll]
  (seq (difference (set (range 1 10)) coll)))

(defn empty-coords-in-box [coll br bc]
  (map
   #(let [coord (grid-coord 9 %)]
      [(+ (coord 0) br) (+ (coord 1) bc)])
   (missing-idxs coll)))

(defn impossibles [g r c]
  (remove nil? (distinct (flatten (list (nth-col g c)
			    (nth-row g r)
			    (get-box g 9 r c))))))

(defn solve-row [g r]
  (let [ms (missing (nth-row g r))
	midxs (missing-idxs (nth-row g r))
	s (map  #(missing (impossibles g r %)) midxs) 
	value (some #(when (= 1 (count-having s  %)) %) ms)
	c ((zipmap (map #(has? % value) s) midxs) true)]
    (if value
      (grid-assoc g r c value)
      g))) ;; if nothing was found, return the grid like it was


(defn solve-col [g c]
  (let [ms (missing (nth-col g c))
	midxs (missing-idxs (nth-col g c))
	s (map  #(missing (impossibles g % c)) midxs) 
	value (some #(when (= 1 (count-having s %)) %) ms)
	r ((zipmap (map #(has? % value) s) midxs) true)]
    (if value
      (grid-assoc g r c value)
      g)))


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


(defn solve-grid-one-pass [g]
  (let [r (range 9)]
    (reduce solve-box
	    (reduce solve-row
		    (reduce solve-col g
			    r)
		    r)
	    r)))

(defn solve-grid [grid]
  (loop [g grid last-g nil]
    (if (and (has? g nil) (not= last-g g))
      (recur (solve-grid-one-pass g) g)
      g)))