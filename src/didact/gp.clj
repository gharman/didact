(ns didact.gp
  (:use [clojure.walk])
  (:require [didact.functions :as f]))

;;;; A genetic programming system that forms the core learning engine behind
;;;; didact; the approach is basically a port and simplification of Koza's
;;;; GP Kernal, with enhancements to allow some intelligence around data types, 
;;;; removing the seeming restriction of an entire program having to use one
;;;; data type for all arguments and return values.

;;; Constants
;; The maximum depth of new subtrees created by mutation
(def ^:dynamic *max-depth-for-new-subtrees-in-mutants* (ref 10))

;; The maximum depth of new individuals created by crossover
(def ^:dynamic *max-depth-for-individuals-after-crossover* (ref 10))

;; The fraction of the population that will experience fitness proportionate reproduction (with reselection) during each generation
(def ^:dynamic *fitness-proportionate-reproduction-fraction* (ref 0.1))

;; The fraction of the population that will experience crossover at any point in the tree (including terminals) during each generation
(def ^:dynamic *crossover-fraction* (ref 0.2))

;; Can be any one of :grow, :full, :ramped-half-and-half
(def ^:dynamic *method-of-generation* (ref :ramped-half-and-half))

;; The maximum depth for individuals of the initial random generation
(def ^:dynamic *max-depth-for-new-individuals* (ref 7))

;;; General utilities
(defmacro let-eval
  "Use let-eval to bind to variables/symbols inside an evaluated expression."
  [bindings expr]
  (let [binding-forms (map #(list `quote %) (take-nth 2 bindings))
	expr-forms (map #(list `list ``quote %) (take-nth 2
                                                          (rest bindings)))]
    `(eval (list 'let [~@(interleave binding-forms expr-forms)] ~expr))))

;;; Data structures
(defstruct individual
  :program
  :from
  :standardized-fitness ; From evalution - lower is better
  :adjusted-fitness ; 1/(1+SF)
  :normalized-fitness ; higher is better - SF/sum(AF)
  :hits)

(defn new-individual
  "Create a new individual from a given program"
  [program & from]
  (let [fr (if (nil? from)
	     :unknown
	     (first from))]
    (struct individual program fr 0 0 0 0)))

;;; Program tree manipulation
(defn count-crossover-points
  "Counts the number of points in the tree (program).
   This includes functions as well as terminals."
  [program]
  (if (seq? program)
    (+ 1 (reduce + (map count-crossover-points (rest program)))) 1))

(defn choose-from-weighted-set
  "Choose a random item from an input set, which is a map of items to integer weights.
   A higher weight will select that item proportionally."
  [weighted-set]
  (let [weighted-set (mapcat #(if (vector? %1)
					 (let [item (first %1)
					       weight (second %1)]
					   (repeat weight item))
					 (list %1)) weighted-set)
	choice (nth weighted-set (rand-int (count weighted-set)))]
    choice))

(defn choose-from-terminal-set
  "Choose a terminal function from a weighted set and then evaluate it to produce a terminal value"
  [terminal-set]
  (let [terminal-function (choose-from-weighted-set terminal-set)]
    (terminal-function)))

(def create-individual-program)
(defn create-arguments-for-function
  "Creates the argument list for a node in the tree.
   Number-of-arguments is the number of arguments still remaining to be created.
   Each argument is created in the normal way using create-individual-program."
  [number-of-arguments function-set terminal-set allowable-depth full?]
    (for [index (range number-of-arguments)]
      (create-individual-program function-set terminal-set allowable-depth false full?)))

(defn create-individual-program
  "Creates a program recursively using the specified functions and terminals.
   Allowable depth is the remaining depth of the tree we can create, when we hit zero we will only select terminals.
   Top-node? is true only when we are being called as the top node in the tree.
   This allows us to make sure that we always put a function at the top of the tree.
   Full? indicates whether this individual is to be maximally bushy or not."
  ([function-set terminal-set allowable-depth full?] (create-individual-program function-set terminal-set allowable-depth true full?))
  ([function-set terminal-set allowable-depth top-node? full?]
     (cond (<= allowable-depth 0)
           ;; We've reached maxdepth, so just pick a terminal
           (choose-from-terminal-set terminal-set)
           (or full? top-node?)
           ;; We are the top node or a full tree, so pick only a function
           (let [function (choose-from-weighted-set function-set)]
             (cons function 
                   (create-arguments-for-function (f/arity function)
                                                  function-set terminal-set 
                                                  (- allowable-depth 1) full?)))
           :else
           ;; Choose one from the bag of functions and terminals.
           (let [choice (rand-int (+ (count terminal-set) (count function-set)))]
             (if (< choice (count function-set))
               ;; We chose a function, so pick it out and go on creating the tree down from here.
               (let [function (choose-from-weighted-set function-set)]
                 (cons function (create-arguments-for-function (f/arity function)
                                                               function-set terminal-set
                                                               (- allowable-depth 1) full?)))
               ;; We chose an atom, so pick it out
               (choose-from-terminal-set terminal-set))))))

(defmacro prewalk-swappable-nodes
  "Implementation of prewalk that skips the first symbol in each subtree (the
   functor). All functions have access to @node-index. Note, that this won't
   be very smart about double parens e.g. ((+ 2 2)), but we shouldn't run across
   that case here."
  [f form]
  `(let [~'node-index (ref -1)
	~'last-was-form (ref false)]
     (prewalk (fn [~'x] (dosync
			 (let [~'result 
			       (if (not @~'last-was-form)
				 (do (alter ~'node-index inc)
				     (~f ~'x)) 
				 (do (ref-set ~'last-was-form false) ~'x))]
			   (when (seq? ~'x) (ref-set ~'last-was-form true))
			   ~'result)))
		~form)))

(defn get-subtree
  "Navigate to the subtree node that is numbered by index. We number
   left-to-right, depth first, with the root node being index 0."
  [tree index]
  (let [result (ref ())]
    (prewalk-swappable-nodes
     #(dosync
       (cond (> @node-index index)
	     () ; Terminate when we have an answer
	     (= @node-index index)
	     (ref-set result %1)
	     :else
	     %1)) tree)
    @result))

(defn replace-subtree
  "Replace the portion of a tree at index with subtree"
  [tree subtree index]
  (prewalk-swappable-nodes
   #(if (= @node-index index)
      subtree
      %1) tree))

(defn mutate
  "Mutates the argument program by picking a random point in the tree and substituting
  a brand new subtree created in the same way that we create the initial random population"
  [program function-set terminal-set]
  (let [ ;; Pick the mutation point
	mutation-point (rand-int (count-crossover-points program))
	;; Create a brand new subtree
	new-subtree (create-individual-program
                     function-set terminal-set
                     @*max-depth-for-new-subtrees-in-mutants* nil)]
    ;; Replace the mutation point with the new subtree
    (replace-subtree program new-subtree mutation-point)))

(defn max-depth-of-tree 
  "Returns the depth of the deepest branch of a given tree"
  [tree]
  (if (sequential? tree)
    (+ 1 (if (not (empty? (rest tree)))
	   (apply max (map max-depth-of-tree (rest tree)))
	   0))
    1))

(defn validate-depth
  "Given a new program from a crossover operation, check to see whether we
   have exceeded the maximum allowed depth."
  [new-prog]
  (let [depth (max-depth-of-tree new-prog)]
    (not (or (= 1 depth)
             (> depth @*max-depth-for-individuals-after-crossover*)))))

(defn validate-data-types
  "Given a point in a parent program and a child subtree that is to be appended
   at that point, determine whether the child's return type matches the parent's
   argument type."
  [parent crossover-point child]
  (let [child-type (f/return-type (if (seq? child) (first child) child))
        crossover-form (get-subtree parent crossover-point)
        crossover-atom (if (seq? crossover-form) (first crossover-form) crossover-form)
        parent-type (f/arg-type crossover-atom 0)]
    (= child-type parent-type)))

(defn crossover
  "Performs crossover on the programs at any point in the trees; returns a list of the two new programs."
  [male female]
  (let [male-point (rand-int (count-crossover-points male))
	female-point (rand-int (count-crossover-points female))
	male-fragment (get-subtree male male-point)
	female-fragment (get-subtree female female-point)
        new-male (replace-subtree male female-fragment male-point)
        new-female (replace-subtree female male-fragment female-point)
        valid? (and (validate-data-types male male-point female-fragment)
                    (validate-data-types female female-point male-fragment)
                    (validate-depth new-male) (validate-depth new-female))]
    (if valid?
      (list new-male new-female)
      (list male female))))

;; This is the tournament selection method from Koza's implementation
(defn find-individual 
  "Picks two individuals from the population at random and returns the better one."
  [population]
  (let [population-length (count population)
	individual-a (nth population (rand-int population-length))
	individual-b (nth population (rand-int population-length))]
    (if (< (:standardized-fitness individual-a) (:standardized-fitness individual-b))
      individual-a
      individual-b)))

(defn- new-individual-via-crossover
  "Create a new individual tagged as from crossover"
  [individual]
  (new-individual individual :crossover))

(defn breed-new-population
  "Controls the actual breeding of the new population. Loops through the population
   executing each operation (e.g. crossover, fitness-proportionate reproduction, mutation)
   until it has reached the specified fraction."
  [population function-set terminal-set]
  (loop [population population index 0 result ()]
    (let [population-size (count population)
	  fraction (/ index population-size)
	  individual-1 (find-individual population)]
      (cond (>= index population-size)
	    result
	    (and (< index (- population-size 1))
		 (< fraction @*crossover-fraction*))
	    (let [p1 (:program individual-1)
		  p2 (:program (find-individual population))
		  new-individuals (apply crossover (list p1 p2))]
	      (recur population (+ index 2) (into (map new-individual-via-crossover new-individuals) result)))
	    (< fraction (+ @*fitness-proportionate-reproduction-fraction*
			   @*crossover-fraction*))
	    (recur population (+ index 1) (into (list (new-individual (:program individual-1) :reuse)) result))
	    :else
	    (recur population (+ index 1) (into (list (new-individual (mutate (:program individual-1) function-set terminal-set) :mutation)) result))))))

(defn sort-population-by-fitness
  "Sorts the population according to normalized fitness."
  [population]
  (reverse (sort-by #(get %1 :normalized-fitness) population)))

(defn- set-adjusted-fitness
  "Set the adjusted fitness for an individual"
  [individual]
  (assoc individual :adjusted-fitness
	 (/ 1.0 (+ 1.0 (:standardized-fitness individual)))))

(defn- normalize-fitness-values
  "Utility to normalize fitness values for an individual; used by normalize-fitness-of-population"
  [individual sum-of-adjusted-fitnesses]
  (assoc individual :normalized-fitness 
	 (if (not (= sum-of-adjusted-fitnesses 0) )
	   (/ (:adjusted-fitness individual) sum-of-adjusted-fitnesses)
	   0)))

(defn normalize-fitness-of-population
  "Computes the normalized and adjusted fitness of each individual in the population."
  [population]
  (let [;; Set the adjusted fitness of each member of the population
	adjusted-fitness-population (map set-adjusted-fitness population)
	;; Set the sum of adjusted fitnesses so that we can normalize them
	sum-of-adjusted-fitnesses (reduce + (map :adjusted-fitness adjusted-fitness-population))]
    ;; Create a new population list containing normalized fitness values
    (map normalize-fitness-values adjusted-fitness-population (cycle (list sum-of-adjusted-fitnesses)))))

(defn evaluate-fitness-of-population
  "Loops over the individuals in the population evaluating and recording the fitness and hits."
  [population fitness-cases fitness-function]
  (map (fn [individual]
         (let [fitness-case-results (map (partial fitness-function (:program individual)) fitness-cases)
               standardized-fitness (reduce + (map first fitness-case-results)) 
               hits (reduce + (map second fitness-case-results))]
           (assoc (assoc individual :standardized-fitness standardized-fitness)
             :hits hits))) population))

(defn create-population
  "Creates the population. (Optional) seeded-programs = the first N programs, where N = seed-programs."
  ([population-size function-set terminal-set] (create-population population-size function-set terminal-set ()))
  ([population-size function-set terminal-set seeded-programs]
     (loop [index 0 seeded-program seeded-programs result ()] ;; Index necessary for ramped-half-and-half
       (if (>= index population-size)
         result
         (let [minimum-depth-of-trees 1
               full-cycle? false]
           (if (>= 0 (count seeded-programs))
             ;; Generate a new program
             (let [new-ind (new-individual 
                            (create-individual-program function-set terminal-set
                                                       (cond (or (= @*method-of-generation* :full) (= @*method-of-generation* :grow))
                                                             @*max-depth-for-new-individuals*
                                                             (= @*method-of-generation* :ramped-half-and-half)
                                                             (+ minimum-depth-of-trees (rem index (- @*max-depth-for-new-individuals* minimum-depth-of-trees))))
                                                       true
                                                       (cond (= @*method-of-generation* :full) true
                                                             (= @*method-of-generation* :grow) false
                                                             (= @*method-of-generation* :ramped-half-and-half) full-cycle?)))]
               (recur (inc index) seeded-programs (conj result new-ind)))
             ;; Use a seeded program
             (recur (inc index) (rest seeded-programs) (conj result (first seeded-programs)))))))))

(defn run-gp
  "Loops until the termination predicate indicates a stop or maximum generations are reached"
  ;; External entry point for a new run
  ([population-size maximum-generations fitness-cases fitness-function
    termination-predicate function-set terminal-set]
     (let [population (create-population population-size function-set terminal-set)]
       (run-gp population maximum-generations fitness-cases fitness-function
              termination-predicate function-set terminal-set false nil)))
  ;: External entry with previous best-of-run-individual and old population as seeds
  ([population maximum-generations fitness-cases fitness-function
    termination-predicate function-set terminal-set previous-best-of-run-individual] 
     (run-gp population maximum-generations fitness-cases fitness-function
             termination-predicate function-set terminal-set false previous-best-of-run-individual))
  ;; Internal recursive entry
  ([population generations-left fitness-cases fitness-function
    termination-predicate function-set terminal-set
    end? best-of-run-individual]
     (if end?
       {:best-of-run-individual best-of-run-individual,
        :last-population population} ; Used to continue where we left off
       (let [processed-population (-> (evaluate-fitness-of-population population fitness-cases fitness-function)
                                      normalize-fitness-of-population
                                      sort-population-by-fitness)
             best-of-generation (first processed-population)
             new-population (breed-new-population processed-population function-set terminal-set)
             new-best? (or (nil? best-of-run-individual)
                           (> (:standardized-fitness best-of-run-individual) (:standardized-fitness best-of-generation)))
             best-of-run-individual (if new-best? best-of-generation best-of-run-individual)]
         (recur new-population (- generations-left 1) fitness-cases fitness-function
                termination-predicate function-set terminal-set 
                (or (<= generations-left 0)
                    (termination-predicate (:standardized-fitness best-of-generation) (:hits best-of-generation)))
                best-of-run-individual)))))
