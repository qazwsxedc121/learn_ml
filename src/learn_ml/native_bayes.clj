(ns learn-ml.native-bayes)

(def items
  [{:A "A1" :B "B1" :C "C1" :D "D1" :Label true}
   {:A "A1" :B "B1" :C "C1" :D "D2" :Label true}
   {:A "A2" :B "B1" :C "C2" :D "D1" :Label false}
   {:A "A3" :B "B1" :C "C2" :D "D3" :Label true}
   {:A "A1" :B "B2" :C "C1" :D "D1" :Label false}])

(defn attrs [item]
  "get attribute from dataset"
  (keys (dissoc item :Label)))

(defn p-attr- [t attr d-attr]
  "get items_of_d-attr / items percent form dataset:t with attribute:attr"
  (/
   (count (filter #(= d-attr (% attr)) t))
   (count t)))

(defn decide [t d]
  "given a dataset:t and a data:d give the posibility of the :Label for d"
  (let [t-attrs (attrs (first t))
        d-attrs (map #(d %) t-attrs)
        g (group-by :Label t)
        g-pro (g true)
        g-con (g false)]
    [(reduce * (/ (count g-pro) (count g)) (map #(p-attr- g-pro %1 %2) t-attrs d-attrs))
     (reduce * (/ (count g-con) (count g)) (map #(p-attr- g-con %1 %2) t-attrs d-attrs))]))

(decide items {:A "A1" :B "B1" :C "C1" :D "D1" })

