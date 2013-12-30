(ns kachel.core-test
  (:require [clojure.test :refer :all]
            [kachel.core :refer :all]))

(deftest dense-square-grid-test
  (let [grid (dense-square-grid :width 10 :height 5 :default-fn identity)
        wrapped-grid (dense-square-grid :width 10 :height 5
                                  :wrap-horizontal? true
                                  :wrap-vertical? true
                                  :default-fn identity)]
    (testing "width"
      (is (= 10 (width grid))))
    (testing "height"
      (is (= 5 (height grid))))
    (testing "coordinate->field"
      (are [index coord] (= {:x (coord 0) :y (coord 1) :index index}
                            (coordinate->field grid coord))
           0 [0 0]
           1 [1 0]
           9 [9 0]
           10 [0 1]
           11 [1 1]
           49 [9 4])
      (testing "with out of bounds coordinates"
        (testing "for non-wrapped grid"
          (are [coord] (thrown? AssertionError (coordinate->field grid coord))
               [0 5]
               [1 5]
               [10 0]
               [10 1]
               [-1 0]
               [0 -1]))
        (testing "for wrapped grid"
          (are [index x y coord] (= {:x x :y y :index index}
                                (coordinate->field wrapped-grid coord))
               0 0 0 [0 5]
               1 1 0 [1 5]
               0 0 0 [10 0]
               10 0 1 [10 1]
               9 9 0 [-1 0]
               40 0 4 [0 -1]))))
    (testing "neighbour-coordinate"
      (testing "with non-wrapped grid"
        (is (= nil (neighbour-coordinate grid [0 0] :up)))
        (is (= [0 1] (neighbour-coordinate grid [0 0] :down)))
        (is (= nil (neighbour-coordinate grid [0 0] :left)))
        (is (= [1 0] (neighbour-coordinate grid [0 0] :right)))
        (is (= [9 3] (neighbour-coordinate grid [9 4] :up)))
        (is (= nil (neighbour-coordinate grid [9 4] :down)))
        (is (= [8 4] (neighbour-coordinate grid [9 4] :left)))
        (is (= nil (neighbour-coordinate grid [9 4] :right))))
      (testing "with wrapped grid"
        (is (= [0 4] (neighbour-coordinate wrapped-grid [0 0] :up)))
        (is (= [0 1] (neighbour-coordinate wrapped-grid [0 0] :down)))
        (is (= [9 0] (neighbour-coordinate wrapped-grid [0 0] :left)))
        (is (= [1 0] (neighbour-coordinate wrapped-grid [0 0] :right)))
        (is (= [9 3] (neighbour-coordinate wrapped-grid [9 4] :up)))
        (is (= [9 0] (neighbour-coordinate wrapped-grid [9 4] :down)))
        (is (= [8 4] (neighbour-coordinate wrapped-grid [9 4] :left)))
        (is (= [0 4] (neighbour-coordinate wrapped-grid [9 4] :right)))))
    (testing "neighbour-coordinates"
      (testing "of non-wrapped grid"
        (is (= (set [[5 0] [5 2] [4 1] [6 1]])
               (set (neighbours-coordinates grid [5 1]))))
        (is (= (set [[2 0] [4 0] [3 1]])
               (set (neighbours-coordinates grid [3 0]))))
        (is (= (set [[0 1] [1 0]])
               (set (neighbours-coordinates grid [0 0]))))
        (is (= (set [[8 4] [9 3]])
               (set (neighbours-coordinates grid [9 4])))))
      (testing "of wrapped grid"
        (is (= (set [[5 0] [5 2] [4 1] [6 1]])
               (set (neighbours-coordinates wrapped-grid [5 1]))))
        (is (= (set [[2 0] [4 0] [3 1] [3 4]])
               (set (neighbours-coordinates wrapped-grid [3 0]))))
        (is (= (set [[0 1] [1 0] [9 0] [0 4]])
               (set (neighbours-coordinates wrapped-grid [0 0]))))
        (is (= (set [[8 4] [9 3] [0 4] [9 0]])
               (set (neighbours-coordinates wrapped-grid [9 4]))))))))
