(ns didact.functions-test
  (:require [clojure.test :refer :all]
            [didact.gp :refer :all]
            [didact.functions :refer :all]))

(deftest test-functions-and-terminals
  (testing "arity"
    (is (= 2 (arity div)))
    (let [div2 div] ; Test bound function
      (is (= 2 (arity div2)))))
  (testing "data-types"
    (is (= Number (return-type div)))
    (is (= Number (arg-type div 0)))
    (let [div2 div] ; Test bound function
      (is (= Number (return-type div2)))))
  (testing "didact-function?"
    (is (= true (didact-function? add)))
    (is (= false (didact-function? +))))
  (testing "name"
    (is (= 'add (fname add)))))



