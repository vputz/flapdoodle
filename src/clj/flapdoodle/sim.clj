(ns sim
  (:require
   [complex.core :as c]
   [clojure.core.matrix.complex :as cm]
   [clojure.core.matrix :as m]
   [clojure.core.matrix.implementations :as mi]
   [clojure.core.matrix.selection :as sel]
   [clojure.math.numeric-tower :as math]
   [clojure.pprint :as pp]
   )
  )

                                        ; we don't actually use this
                                        ;(m/set-current-implementation cm/canonical-complex-array)
;;## A basic Quantum Computing simulator
;;
;; What follows is a very basic quantum computer simulator in the form
;; of a vector of complex probabilities.  The state of the system is
;; modified by applying basic unitary operations to the vector through
;; sparse matrices (usually applied via iterative procedures).

(defn complex-tostring
  "A simple display of a complex number in the form [a + bi]"
  [x]
  (str "["(c/real-part x) " + " (c/imaginary-part x) "i]"))

(defn complexarray-tostring
  "Prints a complex array; broken at the moment as it only correctly
  represents 1d complex vectors; will have to dig through core.matrix
  display routines to correctly display multidimensional matrices.
  Done because `complex.core` represents complex arrays as one array
  of the real part and a second array of the imaginary part, and I
  found this difficult to understand in practice."
  [x]
  (apply str "#complex/complex-array ["
         (apply str (interpose ", " (map complex-tostring
                                         (map #(apply c/complex %) (map vector (cm/real x) (cm/imag x))))))
         "]"))
                                        ; basics of a qbit

;; some scratchpad code; delete at some point
(def b0 (m/matrix :ndarray [(c/complex 1 0) (c/complex 0 0)]))
(def rX (m/matrix :ndarray [[0 1] [1 0]]))
(def iX (m/matrix :ndarray [[0 0] [0 0]]))
(def X (cm/complex-array rX iX))
(def X (m/matrix  [[(c/complex 0 0) (c/complex 1 0)]
                   [(c/complex 1 0) (c/complex 0 0)]]))

(def rX (m/matrix [[0 1] [1 0]]))
(def iX (m/matrix [[0 0] [0 0]]))
(def X (cm/complex-array rX iX))
(def b0 (m/mutable (cm/complex-array [1 0] [0 0])))

(defrecord Sim [bindings states])

;; ## Creating and manipulating a ground state

(defn ground-state
  "Creates a ground state with the requested number of bits; the
  length of the vector is 2^nbits (in other words, a three-bit machine
  has 8 possible states).  Starts in the |0> state."
  [num-bits]
  (let [nstates (math/expt 2 num-bits)
        imag-parts (m/zero-vector nstates)
        real-parts (m/mset imag-parts 0 1.0)
        result (m/matrix
                :ndarray
                (map #(apply c/complex %)
                     (map vector real-parts imag-parts)))
        ]
    result))

(defn num-states
  "The number of states represented by a qubit string (just the number of possible states)"
  [qubits]
  (m/ecount qubits))

(defn num-bits-in-qubits
  "The number of bits represented by a qubit string (log2 of the number of possible states)"
  [qubits]
  (let [n (num-states qubits)]
    (int (/ (Math/log n) (Math/log 2)))))

;; ## Representing a qubit string to the user
;;
;; While a real quantum computer cannot give you any information until
;; you measure it (at which point the quantum wavefunction collapses
;; and you can get no further information) it is useful when learning
;; to query the state of the qubits at any time.  Since the actual
;; complex amplitudes are difficult for a beginner to understand, we
;; will print the name of each state followed by its probability
;; (square of the amplitude) and the "angle" of the complex amplitude.
;;
;; In other words, a two-bit quantum computer with an equal chance of
;; being in the state |00> and |11> might be printed as
;;
;;```
;;|00> 0.50 @ 00
;;|01> 0.00 @ 00
;;|10> 0.00 @ 00
;;|11> 0.50 @ 00
;;```

(defn state-name
  "Generate the 'name' of a state (ie state 1 of a two-bit system is
  |01>)"
  [state-number num-bits]
  (->> state-number
     (java.lang.Integer/toBinaryString)
     (pp/cl-format nil (str "~" num-bits ",1,0,'0@A") )
     )
  )

(defn prob-state
  "The probability of a state (the square of its complex amplitude)"
  [qubits state-number]
  (let [state (m/mget qubits state-number)]
    (math/expt (c/abs state) 2)))

(defn prob-state-string
  "A string representing the name of a state followed by its current
  probability and angle"
  [qubits state-number]
  (let [state (m/mget qubits state-number)
        num-bits (num-bits-in-qubits qubits)
        state-name (state-name state-number num-bits)]
    (str "|" state-name ">: " (prob-state qubits state-number) " @" (c/argument state)))
  )

(defn qubits-prob-state-string
  "One long string holding all the state strings; a textual
  representation of the current state of the machine"
  [qubits]
  (let [indexed-states (map-indexed vector qubits)
        state-strings (map #(prob-state-string qubits (first %)) indexed-states)
        interposed (interpose (str \newline) state-strings)]
    (apply str interposed)))

(defn lsb
  "least significant bit"
  [n]
  (bit-and n 1))

(defn lsb-set
  "Whether the rightmost bit of a number is set"
  [n]
  (= 1 (lsb n)))

(defn microstate-number
  "A bit of a funny name for this and it may change.  Basically when
  we're looking at a bunch of states but we're doing an operation on
  only one or more bits, we need to isolate all the cases where those
  bits have discrete 'microstates' (for example, for one bit |0> and
  |1>) but *all other bits* in the QC's qubits are the same.  This can
  be done by taking a state index and shifting each bit of the number
  by the offset of each bit.

  Here, `bits` is a sequence of bits MSB->LSB and n is the 'microstate'
  number"
  [bits microstate]
  (loop [rbits (reverse bits)
         ms microstate
         result 0]
    (if (empty? rbits)
      result
      (recur (rest rbits)
             (bit-shift-right ms 1)
             (bit-or result (bit-shift-left (lsb ms) (first rbits)))))))

(defn apply-matrix-to-states
  "applies an arbitrary matrix to the specified states in the qubits
  vector.  Typically you will not call this directly, instead
  applying the matrix to bits (see `apply-matrix-to-bits`"
  [qubits states mat]
  (let [this-vec (sel/sel qubits states)
        result (m/inner-product mat this-vec)]
    (sel/set-sel! qubits states result)
    qubits))

(defn microstates
  "Calculates the sets of 'microstates' necessary for applying a
  matrix to a set of bits in a qubit vector"
  [num-qubits bits]
  (let [all-bits (reverse (range num-qubits))
        offset-bits (remove #(contains? (set bits) %) all-bits)
        num-offsets (math/expt 2 (count offset-bits))
        offsets (map #(microstate-number offset-bits %) (range num-offsets))
        num-microstates (math/expt 2 (count bits))
        microstates (map #(microstate-number bits %) (range num-microstates))
        all-microstates (map (fn [offset] (map (fn [x] (+ offset x)) microstates)) offsets)]
    all-microstates))

(defn apply-matrix-to-bits
  "Applies an arbitrary square matrix to the specified bits in the
  qubits vector"
  [qubits bits mat]
  (let [num-qubits (num-bits-in-qubits qubits)
        all-microstates (microstates num-qubits bits)]
    (reduce (fn [qbs mstate] (apply-matrix-to-states qbs mstate mat)) qubits all-microstates)
    ))

(def g (ground-state 2))
(apply-matrix-to-bits g [0] H-mat)
(apply-matrix-to-bits g [0 1] CNOT-mat)

(defn bit-pairs
  "Generates all the state numbers representing the 'pairs' of a bit in a 0 or 1 state.
  In each element of such a pair, all the other bits are identical.
  For example, in a two bit system (|00>, |01>, |10>, |11>), the
  `bit-pairs` for bit 0 are [(0,1) (2,3)] while the `bit-pairs` for
  bit 1 are [(0,2) (1,3)]"
  [num-states bit-index]
  (let [all-states (range num-states)
        ups (filter #(not (lsb-set (bit-shift-right % bit-index))) all-states)
        downs (filter #(lsb-set (bit-shift-right % bit-index)) all-states)]
    (map vector ups downs)))

(defn apply-bitwise-matrix
  "applies a bitwise (2x2) matrix to the bit number; 0 is the LSB"
  [qubits bit-index mat]
  (let [n (num-states qubits)
        pairs (bit-pairs n bit-index)]
    (loop [pair (first pairs)
           remain (rest pairs)]
      (let [this-vec (sel/sel qubits pair)
            result (m/inner-product mat this-vec)]
        (sel/set-sel! qubits pair result)
        (if (empty? remain)
          qubits
          (recur (first remain) (rest remain)))))))

;; # Basic operators
(def X-mat
  "NOT"
  (cm/complex-array [[0 1]
                     [1 0]]))

(def Y-mat
  (cm/complex-array [[0 0]
                     [0 0]]
                    [[0 -1]
                     [1 0]]))

(def Z-mat
  (cm/complex-array [[1 0]
                     [0 -1]]))

(def H-mat
  "Hadamard"
  (m/scale
   (cm/complex-array [[1 1]
                      [1 -1]])
   (/ 1 (math/sqrt 2))))

(def S-mat
  "Swap"
  (cm/complex-array [[1 0 0 0]
                     [0 0 1 0]
                     [0 1 0 0]
                     [0 0 0 1]]))

(def CNOT-mat
  "Controlled-NOT"
  (cm/complex-array [[1 0 0 0]
                     [0 1 0 0]
                     [0 0 0 1]
                     [0 0 1 0]]))

;; more scratch code
(m/inner-product X-mat (ground-state 1))
(def g (ground-state 2))
(qubits-prob-state-string g)
(apply-bitwise-matrix g 0 H-mat)
(bit-pairs (ground-state 2) 0)
