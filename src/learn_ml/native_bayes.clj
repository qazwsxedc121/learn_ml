(ns learn-ml.native-bayes)

(def items
  [{:A "A1" :B "B1" :C "C1" :D "D1" :Label true}
   {:A "A1" :B "B1" :C "C1" :D "D2" :Label true}
   {:A "A2" :B "B1" :C "C2" :D "D1" :Label false}
   {:A "A3" :B "B1" :C "C2" :D "D3" :Label true}
   {:A "A1" :B "B2" :C "C1" :D "D1" :Label false}])

(group-by :A items)

(defn attrs [item]
  (keys (dissoc item :Label)))

(defn p-attr- [t attr d-attr]
  (/
   (count (filter #(= d-attr (% attr)) t))
   (count t)))

(defn to-p-map [t]
  (let [g (group-by :Label t)
        g-pro (g true)
        g-con (g false)]
    [g-pro g-con]))


(defn decide [t d]
  (let [t-attrs (attrs (first t))
        d-attrs (map #(d %) t-attrs)
        g (group-by :Label t)
        g-pro (g true)
        g-con (g false)]
    [(reduce * (/ (count g-pro) (count g)) (map #(p-attr- g-pro %1 %2) t-attrs d-attrs))
     (reduce * (/ (count g-con) (count g)) (map #(p-attr- g-con %1 %2) t-attrs d-attrs))]))

(decide items {:A "A1" :B "B1" :C "C1" :D "D1" })

