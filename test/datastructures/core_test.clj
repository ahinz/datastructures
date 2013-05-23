(ns datastructures.core-test
  (:use clojure.test
        datastructures.core))

(deftest single-list

  ; 9 : 10 : 13 : 11 : 99 : Nil
  (def l (scons 9 (scons 10 (scons 13 (scons 11 (scons 99 SNil))))))

  (testing "Basic list operations"
    (is (= (stoclj l) [9 10 13 11 99]))
    (is (= (shead l) 9))
    (is (= (shead (stail l)) 10))
    (is (= (stoclj (stail l)) [10 13 11 99])))

  (testing "Reverse"
    (is (= (stoclj (sreverse l)) [99 11 13 10 9])))

  (testing "Get"
    (is (= (sget l 0) 9))
    (is (= (sget l 3) 11)))

  (testing "Insert"
    (is (= (stoclj (sinsert l 5 0)) [5 9 10 13 11 99]))
    (is (= (stoclj (sinsert l 5 1)) [9 5 10 13 11 99]))
    (is (= (stoclj (sinsert l 5 3)) [9 10 13 5 11 99])))

  (testing "Update"
    (is (= (stoclj (supdate l 5 0)) [5 10 13 11 99]))
    (is (= (stoclj (supdate l 5 1)) [9 5 13 11 99]))
    (is (= (stoclj (supdate l 5 3)) [9 10 13 5 99])))

  (testing "Delete"
    (is (= (stoclj (sdelete l 0)) [10 13 11 99]))
    (is (= (stoclj (sdelete l 2)) [9 10 11 99])))

  (testing "Index"
    (is (= (sindex l 10) 1))
    (is (= (sindex l 100) -1))))
