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
(defmacro defgate [gate bits]
  `(do (defn ~gate [bit#]
         (list (keyword gate) bit#))))

;; this works as a basic def of a gate returning a list of (:gate bit-num)
(defn H [bit]
  (list (keyword 'H) bit))
