(ns matasano-crypto.readers-test
  (:require [clojure.test :refer :all]
            [matasano-crypto.readers :as readers]))


(deftest test-read-binary-string
  (testing "it returns the corresponding byte-array"
    (is (= (vec (byte-array [42 1]))
           (vec (readers/read-binary-string "0010101000000001")))))

  (testing "it handles negative values correctly"
    (is (= (vec (byte-array [-1 1]))
           (vec (readers/read-binary-string "1111111100000001")))))

  (testing "it throws an assertion error when not given a string"
    (is (thrown? java.lang.AssertionError (readers/read-binary-string 42))))

  (testing "it throws an assertion error when not given a string of length not divisible by 8"
    (is (thrown? java.lang.AssertionError (readers/read-binary-string "0000"))))

  (testing "it throws an assertion error when given a string with invalid characters"
    (is (thrown? java.lang.AssertionError (readers/read-binary-string "22222222")))))


(deftest test-read-hex-string
  (testing "it returns the corresponding byte-array"
    (is (= (vec (byte-array [73 39 109]))
           (vec (readers/read-hex-string "49276d")))))

  (testing "it handles negative values correctly"
    (is (= (vec (byte-array [-1 39 109]))
           (vec (readers/read-hex-string "FF276d")))))

  (testing "it handles case correctly"
    (is (= (vec (byte-array [-1 39 109]))
           (vec (readers/read-hex-string "ff276d")))))

  (testing "it throws an assertion error when not given a string"
    (is (thrown? java.lang.AssertionError (readers/read-hex-string 42))))

  (testing "it throws an assertion error when not given a string of length not divisible by 2"
    (is (thrown? java.lang.AssertionError (readers/read-hex-string "49F"))))

  (testing  "it throws an assertion error when given a string with invalid characters"
    (is (thrown? java.lang.AssertionError (readers/read-hex-string "49G")))))


(deftest test-read-base64-string
  (testing "it returns the corresponding byte-array"
    (is (= (vec (byte-array [77 97 110]))
           (vec (readers/read-base64-string "TWFu"))))
    (is (= (vec (byte-array [77 11 15]))
           (vec (readers/read-base64-string "TQsP")))))

  (testing "it handles negative values correctly"
    (is (= (vec (byte-array [-1 -31 110]))
           (vec (readers/read-base64-string "/+Fu")))))

  (testing "it handles padding correctly"
    (is (= (vec (byte-array [77 97 110 1]))
           (vec (readers/read-base64-string "TWFuAQ=="))))
    (is (= (vec (byte-array [77 97 110 1 1]))
           (vec (readers/read-base64-string "TWFuAQE=")))))

  (testing "it throws an assertion error when not given a string"
    (is (thrown? java.lang.AssertionError (readers/read-base64-string 42))))

  (testing "it throws an assertion error when not given a string of length not divisible by 4"
    (is (thrown? java.lang.AssertionError (readers/read-base64-string "TWF"))))

  (testing  "it throws an assertion error when given a string with invalid characters"
    (is (thrown? java.lang.AssertionError (readers/read-base64-string "TWF'")))))


(deftest test-read-ASCII-string
  (testing "it returns the corresponding byte-array"
    (is (= (vec (byte-array [97 98 99]))
           (vec (readers/read-ASCII-string "abc")))))

  (testing "it throws an illegal argument exception when the string contains invalid characters"
    (is (thrown? java.lang.IllegalArgumentException (readers/read-ASCII-string "�a"))))

  (testing "it throws an assertion error when not given a string"
    (is (thrown? java.lang.AssertionError (readers/read-ASCII-string 42)))))
