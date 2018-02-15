(ns test-sim
  (:require  [clojure.test :refer [deftest is use-fixtures]]
             [flapdoodle.sim :as s]))

(defn setup []
  (def ground (s/ground-state 2)))

(defn teardown []
  (ns-unmap *ns* 'ground))

(defn each-fixture [f]
  (setup)
  (f))

(use-fixtures :each each-fixture)

(deftest init
  (is (s/equal-qubits ground
                      (s/from-ket "|00>"))))

(deftest not
  (is (s/equal-qubits (s/apply-matrix-to-bits ground [0] s/X-mat)
                      (s/from-ket "|01>")))
  (is (s/equal-qubits (s/apply-matrix-to-bits ground [1] s/X-mat)
                      (s/from-ket "|11>"))))
