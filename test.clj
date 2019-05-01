(ns test)

;; It's important to use macros for much of flapdoodle, because function
;; arguments are fully evaluated before they're passed to the function, whereas
;; macros receive arguments as unevaluated data

;; this is going to let us determine when and how the macro arguments are
;; evaluated (or just passed down), useful for a DSL

;; when you call a macro... it returns the expanded list (ie (macroexpand my-macro)
;; which is then evaluated

;; let's define a gate, which should act on one or more bits but take other
;; parameterized arguments (for the case of rotations).  Any gate will only work
;; on the same number of bits (we'll rely on currying to do e.g. multiple applications
;; of a Hadamard
(def code-atom (atom []))
(defn emit [op] (swap! code-atom conj op))

(defmacro defgate [gate nbits]
  "Create an unparameterized gate, such as H, CNOT, SWAP, up to 3 bits, usable as (H 0) or (H qvar)"
  (cond
    (= nbits 1)
    `(do (defn ~gate [bit#]
           (emit (list ~(keyword gate) bit#))))
    (= nbits 2)
    `(do (defn ~gate [bit1# bit2#]
           (emit (list ~(keyword gate) bit1# bit2#))))
    (= nbits 3)
    `(do (defn ~gate [bit1# bit2# bit3#]
           (emit (list ~(keyword gate) bit1# bit2#))))))

(defmacro defparmgate [gate nbits]
  "Create a parameterized gate, such as Rx, etc; usable as ((Rx (/ pi 2)) 0) "
  `(do (defn ~gate [param#]
         (cond
           (= ~nbits 1)
           (fn [bit#]
             (emit (list (list ~(keyword gate) param#) bit#)))
           (= ~nbits 2)
           (fn [bit1# bit2#]
             (emit (list (list ~(keyword gate) param#) bit1# bit2#)))))))

(defmacro defq [&form &env name & fdecl])
;; this works as a basic def of a gate returning a list of (:gate bit-num)
(defgate H 1)
(defgate CNOT 2)
(defparmgate Rx 1)

(defn bellpair [b1 b2]
  (H b1)
  (repeat 5 (H b1))
  (CNOT b1 b2))

