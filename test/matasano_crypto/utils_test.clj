(ns matasano-crypto.utils-test
  (:require [clojure.test :refer :all]
            [matasano-crypto.utils :as utils]))


(deftest test-byte-to-string
  (testing "it returns a binary string when given a byte"
    (is (= "00101010"
           (utils/byte-to-string (byte 42)))))

  (testing "it throws an assertion error when not given a byte"
    (is (thrown? java.lang.AssertionError (utils/byte-to-string "42")))))


(deftest test-bytes-to-string
  (testing "it returns a binary string when given a byte-array"
    (is (= "0010101000000001"
           (utils/bytes-to-string (byte-array [(byte 42) (byte 1)])))))

  (testing "it throws an assertion error when not given a byte-array"
    (is (thrown? java.lang.AssertionError (utils/bytes-to-string "42")))))


(deftest test-read-partitioned-hex-string
  (testing "it returns a byte corresponding to the hex string"
    (is (= (byte 127) (utils/read-partitioned-hex-string "7F"))))

  (testing "it uses the full byte and ignores two's compliment"
    (is (= (byte -1) (utils/read-partitioned-hex-string "FF"))))

  (testing "it is case insensitive"
    (is (= (byte -1) (utils/read-partitioned-hex-string "Ff"))))

  (testing "it throws an assertion error when the input is not a string"
    (is (thrown? java.lang.AssertionError (utils/read-partitioned-hex-string 42))))

  (testing "it throws an assertion error when the input is not a string of length two"
    (is (thrown? java.lang.AssertionError (utils/read-partitioned-hex-string "0")))
    (is (thrown? java.lang.AssertionError (utils/read-partitioned-hex-string "000"))))

  (testing "it throws an assertion error when the input contains invalid hex"
    (is (thrown? java.lang.AssertionError (utils/read-partitioned-hex-string "000")))
    (is (thrown? java.lang.AssertionError (utils/read-partitioned-hex-string "0G")))))


(deftest test-read-even-hex-string
  (testing "it returns a byte-array corresponding to the hex string"
    (is (= (vec (byte-array [(byte 73) (byte 39) (byte 109)]))
           (vec (utils/read-even-hex-string "49276d")))))

  (testing  "it throws an assertion error when the input is not a string"
    (is (thrown? java.lang.AssertionError (utils/read-even-hex-string 5)))))


(deftest test-write-hex-string
  (testing "it returns a hex string when given a byte-array"
    (is (= "807f01"
           (utils/write-hex-string (byte-array [(byte -128) (byte 127) (byte 1)])))))

  (testing  "it throws an assertion error when the input is not a string"
    (is (thrown? java.lang.AssertionError (utils/write-hex-string 5)))))


(deftest test-write-base64-string
  (testing "it returns a base64 string when given a byte-array"
    (is (= "TWFu"
           (utils/write-base64-string (byte-array [(byte 77) (byte 97) (byte 110)])))))

  (testing "it adds padding appropriately"
    (is (= "TWFuAQ=="
           (utils/write-base64-string (byte-array [(byte 77) (byte 97) (byte 110) (byte 1)]))))
    (is (= "TWFuAQE="
           (utils/write-base64-string (byte-array [(byte 77) (byte 97) (byte 110) (byte 1) (byte 1)])))))

  (testing "it throws an assertion error when the input is not a byte-array"
    (is (thrown? java.lang.AssertionError (utils/write-base64-string (byte 77))))))


(deftest test-fixed-XOR
  (testing "it returns a byte-array which is the result of XOR operations on corresponding input bytes"
    (is (= (vec (byte-array [(byte -1) (byte 15)]))
           (vec (utils/fixed-XOR (byte-array [(byte -16) (byte -1)]) (byte-array [(byte 15) (byte -16)])))))))
