(ns didact.core
  (:gen-class)
  (:use [clojure.walk])
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
(defrecord Lesson [footprint examples best-function last-population])

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
      (alter *knowledge* conj {~'name (Lesson. ~'footprint () #() ())})
      (intern *ns* ~'name #()))))

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
        (alter *knowledge* update-in [~'name :examples] concat (map eval ~'examples))))))

(defmacro learn ; (basically set up & execute a gp run based on *knowledge* base for a given function name
  "Instruct Didact to work on learning a specific named function that has been taught in the past.
   At the conclusion, the best-function will be interned into the current namespace under the given symbol."
  [name & {:keys [function-set terminal-set] :or {function-set default-function-set,
                                                  terminal-set default-terminal-set}}]
  `(let [~'population-size 50
         ~'max-generations 20
         ~'lesson (get @*knowledge* ~'name)
         ~'footprint (:footprint ~'lesson)
         ~'examples (:examples ~'lesson)
         ~'best-function (:best-function ~'lesson) ;; TODO use
         ~'last-population (:last-population ~'lesson) ;; TODO use
         ~'fitness-cases ~'examples ;; TODO validate examples against footprint
         ~'fitness-function (def-fitness-function
                              (let [value-from-fc (let-eval [
                                                             ;; Do I really need footprint here, actually? I just need # of args? Why do I need types?

                                                             ;; The whole point of binding x is to allow input args to go wherever in the generated program.
                                    ]
                                ) ;; TODO use footprint and Example struct
                              ;; fitness-case is bound
         ~'termination-predicate (gp/def-termination-predicate
                                  (>= ~'best-hits (count ~'fitness-cases)))
         ~'result (gp/run-gp ~'population-size ~'max-generations
                             ~'fitness-cases ~'fitness-function
                             ~'termination-predicate ~'function-set ~'terminal-set) ;; TODO diff arg set if we have a population already in *knowledge* that we want to continue with
         ;; TODO how do we know input/output types of high-lev function? Need to enhance the GP section to cover this - probably result in a change to run-gp's footprint
         ~'best-program (:program (:best-of-run-individual ~'result))
         ~'last-population (:last-population ~'result)] ; Note the popluation is gp/individual structs, not loose functions
     (dosync
      (intern *ns* ~'name ~'best-program)
      (alter *knowledge* assoc-in [name :best-function] ~'best-program)
      (alter *knowledge* assoc-in [name :last-population] ~'last-population))))

(defn recite
  "Have didact print its current implementation of a given function, along with
   any relevant metrics."
  [name]
  (f/pretty-print name))

