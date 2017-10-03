(ns matasano-crypto.writers-test
  (:require [clojure.test :refer :all]
            [matasano-crypto.writers :as writers]))


(deftest test-write-binary-string
  (testing "it returns the corresponding binary string"
    (is (= "0010101000000001"
           (writers/write-binary-string (byte-array [42 1])))))

  (testing "it handles negative values correctly"
    (is (= "1111111100000001"
           (writers/write-binary-string (byte-array [-1 1])))))

  (testing "it throws an assertion error when not given a byte-array"
    (is (thrown? java.lang.AssertionError (writers/write-binary-string 42)))))


(deftest test-write-hex-string
  (testing "it returns the corresponding hex string"
    (is (= "49276d"
           (writers/write-hex-string (byte-array [73 39 109])))))

  (testing "it handles negative values correctly"
    (is (= "ff276d"
           (writers/write-hex-string (byte-array [-1 39 109])))))

  (testing "it throws an assertion error when not given a byte-array"
    (is (thrown? java.lang.AssertionError (writers/write-hex-string 42)))))


(deftest test-write-base64-string
  (testing "it returns the corresponding base64 string"
    (is (= "TWFu"
           (writers/write-base64-string (byte-array [77 97 110])))))

  (testing "it handles negative values correctly"
    (is (= "/+Fu"
           (writers/write-base64-string (byte-array [-1 -31 110])))))

  (testing "it handles padding appropriately"
    (is (= "TWFuAQ=="
           (writers/write-base64-string (byte-array [77 97 110 1]))))
    (is (= "TWFuAQE="
           (writers/write-base64-string (byte-array [77 97 110 1 1])))))

  (testing "it throws an assertion error when not given a byte-array"
    (is (thrown? java.lang.AssertionError (writers/write-base64-string 42)))))


(deftest test-write-ASCII-string
  (testing "it returns the corresponding ASCII string"
    (is (= "abc"
           (writers/write-ASCII-string (byte-array [97 98 99])))))

  (testing "it returns an unknown character when an invalid byte is given"
    (is (= "�a" (writers/write-ASCII-string (byte-array [-1 97])))))

  (testing "it throws an assertion error when not given a byte-array"
    (is (thrown? java.lang.AssertionError (writers/write-ASCII-string 42)))))
