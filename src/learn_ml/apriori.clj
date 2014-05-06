(ns learn-ml.apriori
  (:require [clojure.set]
            [clojure.math.combinatorics :as combo]))

(def dataset
  {"T100" ["I1" "I2" "I5"]
   "T200" ["I2" "I4"]
   "T300" ["I2" "I3"]
   "T400" ["I1" "I2" "I4"]
   "T500" ["I1" "I3"]
   "T600" ["I2" "I3"]
   "T700" ["I1" "I3"]
   "T800" ["I1" "I2" "I3" "I5"]
   "T900" ["I1" "I2" "I3"]})

(def dataset-l
  (vals dataset))

(defn superset? [itemset1 itemset2]
  (let [set1 (set itemset1)
        set2 (set itemset2)]
    (clojure.set/superset? set1 set2)))

(defn union [itemsets]
  (vec (reduce #(clojure.set/union (set %1) (set %2)) itemsets)))

(defn get-support [dataset itemset]
  (count (filter #(superset? % itemset) dataset)))

(defn in-itemsets [x itemsets]
  (let [x-set (set x)
        itemsets (set (map set itemsets))]
    (itemsets x-set)))

(defn childs-in-itemsets [candidate itemsets step]
  (let [childs (combo/combinations candidate step)]
    (every? #(in-itemsets % itemsets) childs)))

(defn connect-1 [itemsets]
  (let [step (count (first itemsets))
        super-itemset (union itemsets)
        candidates (combo/combinations super-itemset (inc step))]
    (filter #(childs-in-itemsets % itemsets step) candidates)))

(defn apriori [dataset support]
  (let [init-item (union dataset)
        init-itemset (map #(identity [%]) init-item)
        init-itemset (filter #(>= (get-support dataset %) support) init-itemset)]
    (loop [result []
           last-itemset init-itemset]
      (let [candidates (connect-1 last-itemset)
            new-itemset (filter #(>= (get-support dataset %) support) candidates)]
        (if (empty? new-itemset)
          result
          (recur (concat result new-itemset) new-itemset))))))
