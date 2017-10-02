(ns matasano-crypto.writers-test
  (:require [clojure.test :refer :all]
            [matasano-crypto.writers :as writers]))


(deftest test-write-binary-string
  (testing "it returns the corresponding binary string"
    (is (= "0010101000000001"
           (writers/write-binary-string (byte-array [(byte 42) (byte 1)])))))

  (testing "it handles negative values correctly"
    (is (= "1111111100000001"
           (writers/write-binary-string (byte-array [(byte -1) (byte 1)])))))

  (testing "it throws an assertion error when not given a byte-array"
    (is (thrown? java.lang.AssertionError (writers/write-binary-string 42)))))


(deftest test-write-hex-string
  (testing "it returns the corresponding hex string"
    (is (= "49276d"
           (writers/write-hex-string (byte-array [(byte 73) (byte 39) (byte 109)])))))

  (testing "it handles negative values correctly"
    (is (= "ff276d"
           (writers/write-hex-string (byte-array [(byte -1) (byte 39) (byte 109)])))))

  (testing "it throws an assertion error when not given a byte-array"
    (is (thrown? java.lang.AssertionError (writers/write-hex-string 42)))))


(deftest test-write-base64-string
  (testing "it returns the corresponding base64 string"
    (is (= "TWFu"
           (writers/write-base64-string (byte-array [(byte 77) (byte 97) (byte 110)])))))

  (testing "it handles negative values correctly"
    (is (= "/+Fu"
           (writers/write-base64-string (byte-array [(byte -1) (byte -31) (byte 110)])))))

  (testing "it handles padding appropriately"
    (is (= "TWFuAQ=="
           (writers/write-base64-string (byte-array [(byte 77) (byte 97) (byte 110) (byte 1)]))))
    (is (= "TWFuAQE="
           (writers/write-base64-string (byte-array [(byte 77) (byte 97) (byte 110) (byte 1) (byte 1)])))))

  (testing "it throws an assertion error when not given a byte-array"
    (is (thrown? java.lang.AssertionError (writers/write-base64-string 42)))))


(deftest test-write-ASCII-string
  (testing "it returns the corresponding ASCII string"
    (is (= "abc"
           (writers/write-ASCII-string (byte-array [(byte 97) (byte 98) (byte 99)])))))

  (testing "it throws an illegal argument exception when a byte is out of range"
    (is (thrown? java.lang.IllegalArgumentException (writers/write-ASCII-string (byte-array [(byte -1)])))))

  (testing "it throws an assertion error when not given a byte-array"
    (is (thrown? java.lang.AssertionError (writers/write-ASCII-string 42)))))



