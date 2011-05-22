(ns kaiketsu.util)


(defn ** [n] (* n n))
(defn sqrt [n] (int (Math/floor (Math/sqrt n))))

;; remove the nth element from l
(defn remove-nth [n l]
  (concat (take n l) (drop (inc n) l)))

;; returns true when x is contained in coll
(defn has? [coll x]
  (some #(= % x) coll))

;; count the number of collections in coll that contain x.
(defn count-having [coll x]
  (count (remove nil?
		 (map #(has? % x) coll))))

