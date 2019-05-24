(ns test)

;; It's important to use macros for much of flapdoodle, because function
;; arguments are fully evaluated before they're passed to the function, whereas
;; macros receive arguments as unevaluated data

;; this is going to let us determine when and how the macro arguments are
;; evaluated (or just passed down), useful for a DSL

;; when you call a macro... it returns the expanded list (ie (macroexpand my-macro)
;; which is then evaluated

;; &form - actual form (as data) being invoked
;; &env map of local bindings at point of macro expansion
;; ' turn off eval for everything that follows; 'x -> (quote x)
;; ` return fully qualified symbols, and allows unquote with ~
;; ~@ unquote splicing, unwraps a seqable data structure and puts contents in the enclosing structure
;; varname# auto-gensym
;; & args in var block binds args to the remainder of the argument list

;; let's define a gate, which should act on one or more bits but take other
;; parameterized arguments (for the case of rotations).  Any gate will only work
;; on the same number of bits (we'll rely on currying to do e.g. multiple applications
;; of a Hadamard
(defmacro defgate [gate nbits]
  "Create an unparameterized gate, such as H, CNOT, SWAP, up to 3 bits, usable as (H 0) or (H qvar)"
  (cond
    (= nbits 1)
    `(do (defn ~gate [bit#]
           (list :gate ~(keyword gate) bit#)))
    (= nbits 2)
    `(do (defn ~gate [bit1# bit2#]
           (list :gate ~(keyword gate) bit1# bit2#)))
    (= nbits 3)
    `(do (defn ~gate [bit1# bit2# bit3#]
           (list :gate ~(keyword gate) bit1# bit2#)))))

(defmacro defparmgate [gate nbits]
  "Create a parameterized gate, such as Rx, etc; usable as ((Rx (/ pi 2)) 0) "
  `(do (defn ~gate [param#]
         (cond
           (= ~nbits 1)
           (fn [bit#]
             (list :parmgate (list ~(keyword gate) param#) bit#))
           (= ~nbits 2)
           (fn [bit1# bit2#]
             (list :parmgate (list ~(keyword gate) param#) bit1# bit2#))))))

(defn gate? [x]
  (and (seq? x)
       (= :gate (first x))))

(defn parmgate? [x]
  (and (seq? x)
       (= :parmgate (first x))))

(defn block? [x]
  (and (seq? x)
       (= :block (first x))))

(defn valid-op? [x]
  (or (gate? x) (parmgate? x) (block? x)))

(defn qseq [& s]
  "Flattens the sequence of operations to base gates"
  (filter valid-op? (tree-seq sequential? seq s)))

;; this works as a basic def of a gate returning a list of (:gate bit-num)
(defgate H 1)
(defgate CNOT 2)
(defparmgate Rx 1)
(defn measure [bit]
  `(:measure ~bit))
(defn block [s & forms]
  (let [qforms (qseq forms)]
    `(:block ~s
             ~@qforms)))

(defn defq [& forms]
  forms)

(defn bellpair [b1 b2]
  (block "bellpair"
         (H b1)
         (CNOT b1 b2)))

(defmacro qcomp [form]
  `(do
     (reset! code-atom [])
     (eval ~form)))
