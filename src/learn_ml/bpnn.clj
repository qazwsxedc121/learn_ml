(ns learn-ml.bpnn)
;; simple 3 layers(input hidden output) back-propagation neural network

(def net
  {:hidden 6
   :input 4
   :output 1})

(defn gen-random-list [len]
  (take len (repeatedly rand)))


(defn gen-weight [net]
  (let [i (net :input)
        h (net :hidden)
        o (net :output)]
    (assoc net
      :weight_level1 (take i (repeatedly #(gen-random-list h)))
      :weight_level2 (take h (repeatedly #(gen-random-list o))))))



(defn sigmoid [x]
  (/ 1
     (+ 1
        (Math/exp (- x)))))

(defn v-v-multiply [x y]
  "vector * vector"
  (reduce + (map * x y)))

(defn v-s-multiply [v s]
  "vector * scalar"
  (map #(* s %) v))

(defn v-plus [x y]
  "vector + vector"
  (map + x y))

(defn calc-layer [in w]
  (reduce v-plus (map #(v-s-multiply %1 %2) w in)))

(def example-w
  [[1 2 3 4]
   [2 3 4 1]
   [3 2 1 4]])

(calc-layer [0.5 0.5 0.5] example-w)

(defn run-net-once [net input]
  (let [w1 (net :weight_level1)
        w2 (net :weight_level2)
        out1 (map sigmoid (calc-layer input w1))]
    (map sigmoid (calc-layer out1 w2))))

(run-net-once (gen-weight net) [0.4 0.4 0.4 0.4])

