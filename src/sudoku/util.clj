(ns sudoku.util)


(defn ** [n] (* n n))
(defn sqrt [n] (int (Math/floor (Math/sqrt n))))

(defn remove-nth [n l]
  (concat (take n l) (drop (inc n) l)))

(defn has? [coll x]
  (some #(= % x) coll))

(defn count-having [coll x]
  (count (remove nil?
		 (map #(has? % x) coll))))

