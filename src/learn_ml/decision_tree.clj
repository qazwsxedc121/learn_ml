(ns learn-ml.decision-tree)

(defn log2 [x]
  "get log2(x)"
  (/
   (Math/log x)
   (Math/log 2)))

(defn entropy [p,q]
  "simple calculation of entropy with consider for zero"
  (if (or (zero? p) (zero? q))
    0
    (let [n (+ p q)
          pn (/ p n)
          qn (/ q n)]
      (-
       (+
        (* pn (log2 pn))
        (* qn (log2 qn)))))))



(defn count-pro-con [t]
  "calculate how may of entry in datatset:t 's :Label is true or false"
  (list
   (count (filter #(= (% :Label) true) t))
   (count (filter #(= (% :Label) false) t))))

(defn set-entropy [t]
  "calculate entropy of a datatset:t"
  (let [[p q] (count-pro-con t)]
    (entropy p q)))

(defn gain [t attr]
  "Information gain of attribute:attr in dataset:t"
  (let [t-c (count t)
        sets0 (vals (group-by attr t))
        sets1 (map count sets0)
        sets2 (map set-entropy sets0)]
    (- (set-entropy t)
       (reduce + (map #(* (/ %1 t-c) %2) sets1 sets2)))))

(defn max-map-kv [m]
  "get biggest value item in a map , return [key value]"
  (reduce #(if (> (second %1) (second %2)) %1 %2)
          m))

(defn set-attrs [t]
  "get attributes in a dataset:t"
  (remove #{:Label} (keys (first t))))

(defn find-attr [t]
  "find attribute in datasets which has biggest information gain"
  (let [attrs  (set-attrs t)
        gains (map #(gain t %) attrs)
        attr-map (zipmap attrs gains)]
    (first (max-map-kv attr-map))))

(defn group-children [t attr process]
  (into {}
        (for [[k v] (group-by attr t)]
          [k (process (map #(dissoc % attr) v))])))

(defn build-id3-tree [t]
  "build a id3 tree with provided datatset:t"
  (cond
   (empty? t) {}
   (every? true? (map :Label t)) {:type :leaf :label true}
   (every? false? (map :Label t)) {:type :leaf :label false}
   (empty? (set-attrs t)) {:type :leaf :label (let [[p q] (count-pro-con t)] (> p q))}
   :else
   (let [attr (find-attr t)
         childs (group-children t attr build-id3-tree)]
     {:type :node
      :attr attr
      :childs childs})))




