(ns conway.core-test
  (:require [clojure.test :refer :all]
            [conway.core :refer :all]))

(deftest grid-test
  (testing "building a grid"
    (is (= (grid 0)
           []))
    (is (= (grid 2)
           [[(cell) (cell)]
            [(cell) (cell)]]))))

(def simple-grid
  [["a" "b" "c"]
   ["d" "e" "f"]
   ["g" "h" "i"]])

(deftest neighbours-test
  (testing "when surrounded by neighbours"
    (is (= (into #{} (neighbours simple-grid 1 1))
           #{"a" "b" "c" "d" "f" "g" "h" "i"})))

  (testing "when partially surrounded by neighbours (on edge)"
    (is (= (into #{} (neighbours simple-grid 0 0))
           #{"b" "d" "e"}))
    (is (= (into #{} (neighbours simple-grid 1 2))
           #{"b" "c" "e" "h" "i"})))

  (testing "when cell position is out of the grid"
    (is (thrown? AssertionError (neighbours simple-grid -1 0)))
    (is (thrown? AssertionError (neighbours simple-grid 0 3))))

  (testing "when grid is empty"
    (is (thrown? AssertionError (neighbours [] 0 1)))))

(deftest transit-test
  (testing "live cell with fewer than two live neighbours dies, as if caused by under-population"
    (is (= (transit [[{:alive true}]])
           [[{:alive false}]]))
    (is (= (transit [[{:alive true} {:alive true}]])
           [[{:alive false} {:alive false}]])))

  (testing "live cell with two or three live neighbours lives on to the next generation"
    (is (= (transit [[{:alive true} {:alive true} {:alive true}]])
           [[{:alive false} {:alive true} {:alive false}]]))
    (is (= (transit [[{:alive true} {:alive true}]
                     [{:alive true} {:alive true}]])
           [[{:alive true} {:alive true}]
            [{:alive true} {:alive true}]])))

  (testing "live cell with more than three live neighbours dies, as if by overcrowding"
    (is (= (transit [[{:alive true} {:alive true}]
                     [{:alive true} {:alive true}]
                     [{:alive true} {:alive true}]])
           [[{:alive true}  {:alive true}]
            [{:alive false} {:alive false}]
            [{:alive true}  {:alive true}]])))

  (testing "dead cell with exactly three live neighbours becomes a live cell, as if by reproduction"
    (is (= (transit [[{:alive false} {:alive true}]
                     [{:alive true}  {:alive true}] ])
           [[{:alive true} {:alive true}]
            [{:alive true} {:alive true}]])))
  )
