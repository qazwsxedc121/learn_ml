(ns learn-ml.decision-tree)

(defn log2 [x]
  (/
   (Math/log x)
   (Math/log 2)))

(defn entropy [p,q]
  (if (or (zero? p) (zero? q))
    0
    (let [n (+ p q)
          pn (/ p n)
          qn (/ q n)]
      (-
       (+
        (* pn (log2 pn))
        (* qn (log2 qn)))))))

(entropy 9 5)

(entropy 0 9)

(def items
  [{:A "A1" :B "B1" :C "C1" :D "D1" :Label true}
   {:A "A1" :B "B1" :C "C1" :D "D2" :Label false}
   {:A "A2" :B "B1" :C "C2" :D "D1" :Label false}
   {:A "A3" :B "B1" :C "C2" :D "D3" :Label true}
   {:A "A1" :B "B1" :C "C1" :D "D1" :Label false}])

(group-by :A items)

(remove #{:A} '(:A :B :C))

(map #(+ %1 %2) '(1 2 3) '(2 4 6))

(defn count-pro-con [t]
  (list
   (count (filter #(= (% :Label) true) t))
   (count (filter #(= (% :Label) false) t))))

(defn set-entropy [t]
  (let [[p q] (count-pro-con t)]
    (entropy p q)))

(defn gain [t attr]
  (let [t-c (count t)
        sets0 (vals (group-by attr t))
        sets1 (map count sets0)
        sets2 (map set-entropy sets0)]
    (- (set-entropy t)
       (reduce + (map #(* (/ %1 t-c) %2) sets1 sets2)))))

(defn max-map-kv [m]
  (reduce #(if (> (second %1) (second %2)) %1 %2)
          m))

(defn set-attrs [t]
  (remove #{:Label} (keys (first t))))

(defn find-attr [t]
  (let [attrs  (set-attrs t)
        gains (map #(gain t %) attrs)
        attr-map (zipmap attrs gains)]
    (first (max-map-kv attr-map))))

(defn group-children [t attr process]
  (into {}
        (for [[k v] (group-by attr t)]
          [k (process (map #(dissoc % attr) v))])))

(defn build-id3-tree [t]
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

(find-attr items)

(build-id3-tree items)



