(ns matasano-crypto.utils-test
  (:require [clojure.test :refer :all]
            [matasano-crypto.utils :as utils]))


(deftest test-byte-to-string
  (testing "it returns a binary string when given a byte"
    (is (= "00101010" (utils/byte-to-string (byte 42)))))

  (testing "it throws an assertion error when not given a byte"
    (is (thrown? java.lang.AssertionError (utils/byte-to-string "42")))))


(deftest test-bytes-to-string
  (testing "it returns a binary string when given an array of bytes"
    (is (= "0010101000000001" (utils/bytes-to-string (byte-array [(byte 42) (byte 1)])))))

  (testing "it throws an assertion error when not given an array of bytes"
    (is (thrown? java.lang.AssertionError (utils/bytes-to-string "42")))))


(deftest test-read-partitioned-hex-string
  (testing "it returns a byte corresponding to the hex string"
    (is (= (byte 127) (utils/read-partitioned-hex-string "7F"))))

  (testing "it uses the full byte and ignores two's compliment"
    (is (= (byte -1) (utils/read-partitioned-hex-string "FF"))))

  (testing "it is case insensitive"
    (is (= (byte -113) (utils/read-partitioned-hex-string "8f"))))

  (testing "it throws an assertion error when the input is not a string"
    (is (thrown? java.lang.AssertionError (utils/read-partitioned-hex-string 42))))

  (testing "it throws an assertion error when the input is not a string of length two"
    (is (thrown? java.lang.AssertionError (utils/read-partitioned-hex-string "0")))
    (is (thrown? java.lang.AssertionError (utils/read-partitioned-hex-string "000"))))

  (testing "it throws an assertion error when the input contains invalid hex"
    (is (thrown? java.lang.AssertionError (utils/read-partitioned-hex-string "000")))
    (is (thrown? java.lang.AssertionError (utils/read-partitioned-hex-string "0G")))))


#_(deftest test-read-hex-string
  (testing "it returns a collection of bytes corresponding to the hex string"
    (is (= (vec (byte-array [(byte 15) (byte 5)])) (vec (utils/read-hex-string "F5")))))

  (testing "it throws an assertion error when not given a string"
    (is (thrown? java.lang.AssertionError (utils/read-hex-string 5)))))


