(ns learn-ml.decision-tree-test
  (:require [clojure.test :refer :all]
            [learn-ml.decision-tree :refer :all]))

(def items
  [{:A "A1" :B "B1" :C "C1" :D "D1" :Label true}
   {:A "A1" :B "B1" :C "C1" :D "D2" :Label false}
   {:A "A2" :B "B1" :C "C2" :D "D1" :Label false}
   {:A "A3" :B "B1" :C "C2" :D "D3" :Label true}
   {:A "A1" :B "B1" :C "C1" :D "D1" :Label false}])

(deftest a-test
  (testing "build tree error"
    (is (= (build-id3-tree items)
           {:type :node, :attr :A,
            :childs {
                     "A1" {:type :node, :attr :D,
                           :childs {
                                    "D1" {:type :node, :attr :C,
                                          :childs {
                                                   "C1" {:type :node, :attr :B,
                                                         :childs {"B1" {:type :leaf, :label false}}}}},
                                    "D2" {:type :leaf, :label false}}},
                     "A2" {:type :leaf, :label false},
                     "A3" {:type :leaf, :label true}}}))
    (is (= (decide-with-tree (build-id3-tree items) {:A "A2" :B "B1" :C "C2" :D "D3"})
           false))))
