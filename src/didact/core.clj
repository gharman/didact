(ns didact.core
  (:gen-class)
  (:use [clojure.walk]
        [clojure.repl :only (doc source)])
  (:require [didact.functions :as f]
            [didact.gp :as gp]))

;;;; Didact is a program-by-example paradigm in which the interpreter learns and grows, rather than being given explicit direction.
;;;; It is expected that the API laid out in this (main) module may be applied to an arbitrary problem solver; the initial
;;;; implementation features a genetic programming approach.

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!")) ;; TODO either read in and evaluate a file, or drop us into a REPL

;;; Internal data structures
(def ^:dynamic *knowledge* (ref {})) ; Key = symbol; value = lesson struct

(defrecord Footprint [return-type docstring args])
(defrecord Lesson [footprint examples best-function best-fitness last-population])

(def default-terminal-set {f/floating-point-random-constant 1})
(def default-function-set {f/abs 1, f/add 1, f/subtract 1, f/div 1, f/multiply 1}) ;; TODO should intern everything from functions in this NS

(defmacro plan
  "Plan future learning (by declaring a functional footprint.)
   This must be done in order to teach the function.
   Args should be a map of name to datatype."
  [return-type name ^String doc args]
  `(let [~'name (symbol (quote ~name))
         ~'footprint (Footprint. ~return-type ~doc ~args)]
     (dosync
      (alter *knowledge* conj {~'name (Lesson. ~'footprint () () Integer/MAX_VALUE ())}) ; MAX_VALUE is the worst possible fitness (0 is best)
      (intern *ns* ~'name #())))) ;; TODO Dummy function should still use the correct arity

(defn example
  "Create a single example to feed into teach"
  [return-value & arguments]
  {:return-value return-value
   :arguments arguments})

;;; System interaction
;; TODO validate that all examples use the same structure; first one defines this.
(defmacro teach 
  "Teach Didact by giving it one or more examples that demonstrate a successful
   evaluation of the (new) named function. If the function has been taught in
   the past, then this incorporates new examples. Note that this function does
   not give didact the chance to practice/learn, it simply delivers a 'lesson plan'."
  [name & examples] ;; TODO do we need any e.g accuracy information? (add to lesson also then)
  `(let [~'name (symbol (quote ~name))
         ~'examples (quote ~examples)]
     (dosync 
      (if (nil? (get @*knowledge* ~'name)) ; Create the record if it doesn't already exist
        (println "Must plan before teaching.") ;; TODO throw exception
        (alter *knowledge* update-in [~'name :examples] concat (map eval ~'examples)))
      (println "Examples known: " (count (:examples (get @*knowledge* ~'name)))))))

;; TODO we should track an internal function & terminal set, combining defaults and learned functions - not take as args here
(defmacro learn ; (basically set up & execute a gp run based on *knowledge* base for a given function name
  "Instruct Didact to work on learning a specific named function that has been taught in the past.
   At the conclusion, the best-function will be interned into the current namespace under the given symbol."
  [name & {:keys [function-set terminal-set] :or {function-set 'default-function-set,
                                                  terminal-set 'default-terminal-set}}]
  `(let [~'population-size 5 ;; TODO small sizes for development; tune these later
         ~'max-generations 5
         ~'name (quote ~name)
         ~'lesson (get @*knowledge* ~'name)
         ~'footprint (:footprint ~'lesson)
         ~'numargs (count (:args ~'footprint))
         ;; Add arguments to the terminal set (f/argN)
         ~'terminal-set (merge ~terminal-set
                               (into {} (for [~'arg (map #(eval (symbol (str "f/" %))) (f/generate-arg-terminals ~'numargs))]
                                          {~'arg 1})))
         ~'examples (:examples ~'lesson) ;; TODO validate examples against footprint
         ~'best-function (:best-function ~'lesson)
         ~'best-fitness (:best-fitness ~'lesson)
         ~'last-population (:last-population ~'lesson)
         ~'fitness-cases (map #(conj (:arguments %) (:return-value %)) ~'examples) ;; GP engine expects a list, first = return value, rest = args
         ~'fitness-function ~'gp/fitness-function-number-default ;; TODO dispatch on return type
         ~'termination-predicate (gp/def-termination-predicate
                                   (>= ~'best-hits (count ~'fitness-cases)))
         ~'result (if (empty? ~'last-population)
                    (gp/run-gp ~'population-size ~'max-generations
                               ~'fitness-cases ~'fitness-function
                               ~'termination-predicate ~function-set ~'terminal-set)
                    (gp/run-gp ~'last-population ~'max-generations
                               ~'fitness-cases ~'fitness-function
                               ~'termination-predicate ~function-set ~'terminal-set
                               (assoc (gp/new-individual ~'best-function)
                                 :standardized-fitness ~'best-fitness)))
         ;; TODO how do we know input/output types of high-lev function? Need to enhance the GP section to cover this - probably result in a change to run-gp's footprint
         ~'new-best-function (:program (:best-of-run-individual ~'result))
         ~'new-best-fitness (:standardized-fitness (:best-of-run-individual ~'result))
         ~'best-program-executable (f/wrap-program ~'numargs ~'new-best-function)
         ~'last-population (:last-population ~'result)] ; Note the popluation is gp/individual structs, not loose functions
     (println "Fitness of best function (lower is better): " ~'new-best-fitness)
     (dosync
        (alter *knowledge* assoc-in [~'name :best-function] ~'new-best-function)
        (alter *knowledge* assoc-in [~'name :best-fitness] ~'new-best-fitness)
        (alter *knowledge* assoc-in [~'name :last-population] ~'last-population)
        (intern *ns* ~'name ~'best-program-executable)
        nil)))

(defmacro recite
  "Have didact print its current implementation of a given function, along with
   any relevant metrics."
  [name]
  `(f/pretty-print (:best-function (get @*knowledge* '~name))))

(defn tableau
  "List (as a human-readble string) all learned functions"
  []
  (doall
   (for [fu (keys @*knowledge*)]
     (println fu)))
  nil)


  

