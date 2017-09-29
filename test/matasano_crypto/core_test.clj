(ns matasano-crypto.core-test
  (:require [clojure.test :refer :all]
            [matasano-crypto.core :as core]))

(deftest test-challenge-one
  (testing "it satisfies the conditions outlined at: http://cryptopals.com/sets/1/challenges/1"
    (let [challenge-input "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
          challenge-output "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"]
      (is (= challenge-output
             (core/challenge-one challenge-input))))))

(deftest test-challenge-two
  (testing "it satisfies the conditions outlined at: http://cryptopals.com/sets/1/challenges/2"
    (let [challenge-input-a "1c0111001f010100061a024b53535009181c"
          challenge-input-b "686974207468652062756c6c277320657965"
          challenge-output "746865206b696420646f6e277420706c6179"]
      (is (= challenge-output
             (core/challenge-two challenge-input-a challenge-input-b))))))
