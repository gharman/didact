(ns didact.gp-test
  (:require [clojure.test :refer :all]
            [didact.gp :refer :all]
            [didact.functions :refer :all]))

(deftest test-tree-manipulation
  (testing "count-crossover-points"
    (is (= 3 (count-crossover-points '(+ 2 2))))
    (is (= 7 (count-crossover-points '(+ 2 (- 3 5) (+ 1))))))
  (testing "choose-from-weighted-set"
    (let [tset {positive-integer-random-constant 1,
                floating-point-random-constant 0} 
          terminal (choose-from-terminal-set tset)]
      (is (> terminal -1))
      (is (< terminal 11))
      (is (integer? terminal))) ; fprc should never be chosen because of 0 weight
    (let [terminal (defterminal constant "Test a constant-generating terminal" 42)]
      (is (= 42 (terminal)))))
  (testing "subtrees"
    (let [tree '(+ 1 (- 2 (* 3 4)) 5)
          tree2 (replace-subtree tree '(+ 0 7) 2)]
      (is (= -4 (eval tree))) ; Sanity check
      (is (= -10 (eval (get-subtree tree 2))))
      (is (= 13 (eval tree2)))))
  (testing "max-depth-of-tree"
    (let [tree '(+ 1 (- 2 (* 3 4)) 5)]
      (is (= 4 (max-depth-of-tree tree))))))

;; Use for data type validation below
(deffunction String say-hi "Say hello N times" {n Number} (repeat n "hello"))

(deftest test-program-creation
  (testing "create-individual-program"
    (let [function-set {div 1, abs 1}
          terminal-set {integer-random-constant 1}
          allowable-depth 3
          program (create-individual-program function-set
                                             terminal-set
                                             allowable-depth
                                             true)
          result (eval program) ; This is the main point of the test: does this croak?
          result2 (mutate program function-set terminal-set)] ; ...and this.
      (is (number? result)))) ; These functions can only produce a number
  (testing "crossover-validation"
    (let [function-set-str {say-hi 1}
          function-set-num {abs 1}
          terminal-set {integer-random-constant 1}
          program-str (create-individual-program function-set-str terminal-set 3 true)
          program-num (create-individual-program function-set-num terminal-set 3 true)]
      (is (= true (validate-depth program-str)))
      (binding [*max-depth-for-individuals-after-crossover* (ref 2)]
        (is (= false (validate-depth program-str))))
      (is (= true (validate-data-types program-num 1 (get-subtree program-num 1))))
      (is (= false (validate-data-types program-str 1 (get-subtree program-str 1))))))
  (testing "crossover"
    (let [function-set {div 1, abs 1}
          terminal-set {integer-random-constant 1}
          allowable-depth 8
          male (create-individual-program function-set
                                             terminal-set
                                             allowable-depth
                                             true)
          female (create-individual-program function-set
                                             terminal-set
                                             allowable-depth
                                             true)
          newprogs (crossover male female)
          malevalue (eval male)
          newmalevalue (eval (first newprogs))]
      (is (number? newmalevalue)))) ; Not a great test, just making sure it didn't croak - hard to make this reproducible
  (testing "populations"
    (let [function-set {div 2, abs 1}
          terminal-set {integer-random-constant 1}
          population (create-population 5 function-set terminal-set)]
      (is (= 5 (count population)))
      (is (number? (eval (:program (first population)))))
      (let [newpop (breed-new-population population function-set terminal-set)]
        (is (= 5 (count newpop)))))))

(defstruct regression-fitness-case
  :independent-variable
  :target)

(deftest test-gp-system
  (testing "gp-system"
    ;; Use a regression analysis
    (let [function-set {abs 1, add 1, subtract 1, div 1, multiply 1}
          terminal-set {integer-random-constant 1}
          number-of-fitness-cases 5
          fitness-cases (for [index (range number-of-fitness-cases)]
                            (let [x  (/ index number-of-fitness-cases)]
                              (struct regression-fitness-case
                                      x
                                      (* 0.5 x x))))
          fitness-function (fn [program fitness-case]
                             (let [target-value (:target fitness-case)
                                   iv (:independent-variable fitness-case)
                                   value-from-program (let-eval [x iv] program)
                                   difference (abs (- target-value value-from-program))]
                               (list difference (if (< difference 0.01) 1 0))))
          termination-predicate (fn [best-standardized-fitness best-hits]
                                  (>= best-hits number-of-fitness-cases))
          result (run-gp 3 3 fitness-cases fitness-function
                         termination-predicate function-set terminal-set)]
      (println (pretty-print (:program (:best-of-run-individual result))))
      (is (number? (eval (:program (:best-of-run-individual result)))))))) 

;; TODO f-cases, f-function and t-pred are too complex - we need some sort of generator to simplify! Include the need for the fitness case struct (remove if not required)
