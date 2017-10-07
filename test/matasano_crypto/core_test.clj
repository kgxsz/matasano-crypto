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


(deftest test-challenge-three
  (testing "it satisfies the conditions outlined at: http://cryptopals.com/sets/1/challenges/3"
    (let [challenge-input "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
          challenge-output "Cooking MC's like a pound of bacon"]
      (is (= challenge-output
             (core/challenge-three challenge-input))))))


(deftest test-challenge-four
  (testing "it satisfies the conditions outlined at: http://cryptopals.com/sets/1/challenges/4"
    (let [challenge-output "Now that the party is jumping\n"]
      (is (= challenge-output
             (core/challenge-four))))))


(deftest test-challenge-five
  (testing "it satisfies the conditions outlined at: http://cryptopals.com/sets/1/challenges/5"
    (let [challenge-input-key "ICE"
          challenge-input-plaintext "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
          challenge-output "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"]
      (is (= challenge-output
             (core/challenge-five challenge-input-key challenge-input-plaintext))))))


(deftest test-challenge-six
  (testing "it satisfies the conditions outlined at: http://cryptopals.com/sets/1/challenges/6"
    (let [challenge-output "Terminator X: Bring the noise"]
      (is (= challenge-output
             (core/challenge-six))))))


(deftest test-challenge-seven
  (testing "it satisfies the conditions outlined at: http://cryptopals.com/sets/1/challenges/7"
    (let [challenge-output (slurp "resources/challenge-seven-output.txt")]
      (is (= challenge-output
             (core/challenge-seven))))))
