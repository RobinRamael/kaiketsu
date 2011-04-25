(ns sudoku.test.grid
  (:use [sudoku.grid])
  (:use [clojure.test]))

(deftest test-make-grid
  (is (= (* 9 9) (count (make-grid 9)))))

(deftest test-grid-assoc-nth
  (is (= 5 (grid-nth (grid-assoc (make-grid 9) 3 4 5) 3 4))))