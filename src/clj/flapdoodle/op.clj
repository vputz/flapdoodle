(ns flapdoodle.op
  (:require
   [clojure.spec.alpha :as s]
   [clojure.core.matrix :as m]
   [clojure.core.matrix.complex :as cm]
   [clojure.math.numeric-tower :as math])
  (:import [clojure.core.matrix.complex ComplexArray]))


(s/def ::mat (s/and #(instance? ComplexArray %)
                   square-matrix?))

(s/def ::inverse (s/and  #(instance? ComplexArray %)
                        square-matrix?))

(defn square-matrix? [m] (let [shape (m/shape m)]
                           (and 
                            (= 2 (count shape))
                            (= (first shape) (second shape)))))

(s/def ::op
  (s/and
   (s/keys :req-un [::mat ::inverse])
   ))

(s/valid? ::op {:mat (cm/complex-array [[1 0] [0 -1]]) :inverse (cm/complex-array [[1 2] [3 4]])})

(defrecord Op [mat inverse])

(defn self-inverse-gate [mat]
  (Op. mat mat))

(def X
  "NOT gate (Pauli-X)"
  (self-inverse-gate (cm/complex-array [[0 1]
                                        [1 0]])))

(def Y
  "Pauli-Y"
  (self-inverse-gate (cm/complex-array [[0 0]
                                        [0 0]]
                                       [[0 -1]
                                        [1 0]])))

(def Z
  "Pauli-Z"
  (self-inverse-gate (cm/complex-array [[1 0]
                                        [0 -1]])))

(def H
  "Hadamard gate"
  (self-inverse-gate
   (m/scale
    (cm/complex-array [[1 1]
                       [1 -1]])
    (/ 1 (math/sqrt 2)))))

(def S
  "Swap gate"
  (self-inverse-gate
   (cm/complex-array [[1 0 0 0]
                      [0 0 1 0]
                      [0 1 0 0]
                      [0 0 0 1]])))

(def CNOT
  "Controlled-NOT gate"
  (cm/complex-array [[1 0 0 0]
                     [0 1 0 0]
                     [0 0 0 1]
                     [0 0 1 0]]))

