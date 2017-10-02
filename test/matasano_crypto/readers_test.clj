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
    (is (thrown? java.lang.AssertionError (readers/read-binary-string "0000"))))

  (testing "it throws an assertion error when given a string with invalid characters"
    (is (thrown? java.lang.AssertionError (readers/read-binary-string "22222222")))))


(deftest test-read-hex-string
  (testing "it returns the corresponding byte-array"
    (is (= (vec (byte-array [(byte 73) (byte 39) (byte 109)]))
           (vec (readers/read-hex-string "49276d")))))

  (testing "it handles negative values correctly"
    (is (= (vec (byte-array [(byte -1) (byte 39) (byte 109)]))
           (vec (readers/read-hex-string "FF276d")))))

  (testing "it handles case correctly"
    (is (= (vec (byte-array [(byte -1) (byte 39) (byte 109)]))
           (vec (readers/read-hex-string "ff276d")))))

  (testing "it throws an assertion error when not given a string"
    (is (thrown? java.lang.AssertionError (readers/read-hex-string 42))))

  (testing "it throws an assertion error when not given a string of length not divisible by 2"
    (is (thrown? java.lang.AssertionError (readers/read-hex-string "49F"))))

  (testing  "it throws an assertion error when given a string with invalid characters"
    (is (thrown? java.lang.AssertionError (readers/read-hex-string "49G")))))


(deftest test-read-base64-string
  (testing "it returns the corresponding byte-array"
    (is (= (vec (byte-array [(byte 77) (byte 97) (byte 110)]))
           (vec (readers/read-base64-string "TWFu")))))

  (testing "it handles negative values correctly"
    (is (= (vec (byte-array [(byte -1) (byte -31) ( byte 110)]))
           (vec (readers/read-base64-string "/+Fu")))))

  (testing "it handles padding correctly"
    (is (= (vec (byte-array [(byte 77) (byte 97) (byte 110) (byte 1)]))
           (vec (readers/read-base64-string "TWFuAQ=="))))
    (is (= (vec (byte-array [(byte 77) (byte 97) (byte 110) (byte 1) (byte 1)]))
           (vec (readers/read-base64-string "TWFuAQE=")))))

  (testing "it throws an assertion error when not given a string"
    (is (thrown? java.lang.AssertionError (readers/read-base64-string 42))))

  (testing "it throws an assertion error when not given a string of length not divisible by 4"
    (is (thrown? java.lang.AssertionError (readers/read-base64-string "TWF"))))

  (testing  "it throws an assertion error when given a string with invalid characters"
    (is (thrown? java.lang.AssertionError (readers/read-base64-string "TWF'")))))


(deftest test-read-ASCII-string
  (testing "it returns the corresponding byte-array"
    (is (= (vec (byte-array [(byte 97) (byte 98) (byte 99)]))
           (vec (readers/read-ASCII-string "abc")))))

  (testing "it throws an illegal argument exception when the string contains invalid characters"
    (is (thrown? java.lang.IllegalArgumentException (readers/read-ASCII-string "�a"))))

  (testing "it throws an assertion error when not given a string"
    (is (thrown? java.lang.AssertionError (readers/read-ASCII-string 42)))))
