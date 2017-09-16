(ns matasano-crypto.core-test
  (:require [clojure.test :refer :all]
            [matasano-crypto.core :refer :all]
            [matasano-crypto.utils :as utils]))

(deftest test-print-bits
  (testing "it returns a bit string when given a byte"
    (is (= "00101010" (utils/print-bits (byte 42))))))
