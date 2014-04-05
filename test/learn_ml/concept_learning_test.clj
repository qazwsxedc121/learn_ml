(ns learn-ml.concept-learning-test
  (:require [clojure.test :refer :all]
            [learn-ml.concept-learning :refer :all]))

(deftest a-test

  (testing "i-specializer-than"
    (is (i-specializer-than :phi "s")))

  (testing "find-s"
    (let [fact1 ["1" "2" "3" "4"]
          fact2 ["1" "2" "2" "1"]]
      (is (= (find-s (vector fact1 fact2))
             '("1" "2" :? :?)))))

  (testing "specializer-than"
    (is (specializer-than [:phi :phi "ss"] ["ss" "ss" "ss"])))
)
