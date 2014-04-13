(ns learn-ml.core)

(def schema0
  {:A ["A0" "A1" "A2" "A3"]
   :B ["B0" "B1" "B2"]
   :C ["C0" "C1" "C2"]
   :Label [true false]})

(defn value-map [f m]
  "map-function for a hash-map's value"
  (into {}
        (for [[k v] m]
          [k (f v)])))

(defn random-item-generator [schema]
  (value-map #(rand-nth %) schema))


(defn random-items-generator [schema len]
  "use schema to generate random facts"
  (take len (repeatedly #(random-item-generator schema))))

(random-items-generator schema0 10)


