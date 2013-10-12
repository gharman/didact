(ns didact.functions)

;;;; Library of out-of-the-box functions that ship with didact. This includes
;;;; wrapped versions of basic Clojure functions, and terminal-generating functions.

;;; Utilities to generate and manipulate terminals and functions
(defmacro defterminal [name ^String doc func]
  "Create a terminal-generating function. This can alternately be a simple returnable constant."
  `(defn ~name ~doc [] ~func)) ;; For now, this is just a syntactic wrapper around defn

(defmacro deffunction [return-type name ^String doc args func]
  "Create a function wrapped with sufficient typing data to support heterogenous-typed GP.
   Args should be a map of name to datatype."
  ;; We place argument-type metadata on the function itself, NOT the var,
  ;; otherwise we'll run into trouble when we start re-binding and passing
  ;; the function around during program generation.
  `(def ~name ~(with-meta `(fn [~@(keys args)] ~doc ~func) {:didact-function true :return-type return-type :arg-types (vec (vals args)) :fname (str name)})))

(defmacro return-type [function]
  "Get the data type for a function's return value"
  `(:return-type (meta ~function)))

(defmacro arg-type [function index]
  "Get the data type for the given argument of a function"
  `(nth (:arg-types (meta ~function)) ~index))

(defmacro arity [function]
  "Get the arity of a given function"
  `(count (:arg-types (meta ~function))))

(defmacro fname [function]
  "Get the name of a given function"
  `(:fname (meta ~function)))

(defmacro didact-function? [item]
  "Is the given item a didact function?"
  `(= true (:didact-function (meta ~item))))

(defn pretty-print [program]
  "Pretty print (format, not actually print to out) a program for human readability"
  ;; This is going to be very simple for now - just stick human readable names in the form.
  ;; Nothing fancy like linebreaks or indentation just yet!
  (map #(cond
         (didact-function? %) (fname %)
         (seq? %) (pretty-print %) ; Yeah I know this recursion is troublesome - but we're not exactly lifting heavy here
         :else %) program))

;;; Terminals
;; Note a constant-value terminal generator could be created as so:
;; (defterminal constant-42 "Constant 42" 42)
(defterminal floating-point-random-constant
  "Pick a random number in the range -5.0 to +5.0"
  (float (- (* (rand) 10) 5.0)))

(defterminal integer-random-constant
  "Pick a random number in the range -10 to +10"
  (- (rand-int 21) 10))

(defterminal positive-integer-random-constant
  "Pick a random number in the range 0 to +10"
  (rand-int 11))

;;; Math
(deffunction Number div 
  "Divide-by-zero-safe division"
  {a Number b Number}
  (if (zero? b) 0 (/ a b)))

(deffunction Number abs
  "Absolute value"
  {x Number}
  (if (> 0 x) (- x) x))

(deffunction Number add
  "Addition"
  {a Number b Number}
  (+ a b))

(deffunction Number subtract
  "Subtraction"
  {a Number b Number}
  (- a b))

(deffunction Number multiply
  "Multiplication"
  {a Number b Number}
  (* a b))
