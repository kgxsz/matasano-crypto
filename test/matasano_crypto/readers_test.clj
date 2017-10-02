(ns matasano-crypto.readers-test
  (:require [clojure.test :refer :all]
            [matasano-crypto.readers :as readers]))


(deftest test-read-binary-string
  (testing "it returns the corresponding byte-array"
    (is (= (vec (byte-array [(byte 42) (byte 1)]))
           (vec (readers/read-binary-string "0010101000000001")))))

  (testing "it handles negative values correctly"
    (is (= (vec (byte-array [(byte -1) (byte 1)]))
           (vec (readers/read-binary-string "1111111100000001")))))

  (testing "it throws an assertion error when not given a string"
    (is (thrown? java.lang.AssertionError (readers/read-binary-string 42))))

  (testing "it throws an assertion error when not given a string of length not divisible by 8"
    (is (thrown? java.lang.AssertionError (readers/read-binary-string "0000")))))


