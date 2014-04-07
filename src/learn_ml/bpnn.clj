(ns learn-ml.bpnn)
;; simple 3 layers(input hidden output) back-propagation neural network

(def net
  {:hidden 6
   :input 4
   :output 1})

(defn gen-random-list [len]
  (take len (repeatedly rand)))


(defn gen-weight [net]
  "generate randomize weight for each link"
  (let [i (net :input)
        h (net :hidden)
        o (net :output)]
    (assoc net
      :weight_level1 (take i (repeatedly #(gen-random-list h)))
      :weight_level2 (take h (repeatedly #(gen-random-list o))))))



(defn sigmoid [x]
  "sigmoid function"
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
  "with input vector:in calculate a level output with weight:w before the layer"
  (map sigmoid (reduce v-plus (map #(v-s-multiply %1 %2) w in))))

(def example-w
  [[1 2 3 4]
   [2 3 4 1]
   [3 2 1 4]])

(calc-layer [0.5 0.5 0.5] example-w)



(defn run-net-once-each-layer [net input]
  "compute output for each layer in the neural network"
  (let [w1 (net :weight_level1)
        w2 (net :weight_level2)
        out1 (calc-layer input w1)]
    [out1 (calc-layer out1 w2)]))

(defn run-net-once [net input]
  "test the neural network with net and input"
  (last (run-net-once-each-layer net input)))

(defn delta-output-unit [output target]
  (map #(* %1 (- 1 %1) (- %2 %1)) output target))

(defn delta-hidden-unit [output weight delta-o]
  (map #(* %1 (- 1 %1) (v-v-multiply delta-o %2)) output weight))

(defn weight-update-i [weight-i delta learning-rate input-x]
  (+ weight-i
     (* learning-rate
        input-x
        delta)))

(defn train-net-once [net input target]
  "train a net's weight with input and target output"
  (let [[out1 out2] (run-net-once-each-layer net input)
        delta-2 (delta-output-unit out2 target)
        delta-1 (delta-hidden-unit out1 (:weight_level2 net) delta-2)
        w1 (:weight_level1 net)
        w2 (:weight_level2 net)
        input1 (map #(v-s-multiply %1 %2) w1 input)
        input2 (map #(v-s-multiply %1 %2) w2 out1)]
    (assoc net
      :weight_level1 (map #(map (fn [w d x] (weight-update-i w 0.3 d x)) %1 delta-1 %2) w1 input1)
      :weight_level2 (map #(map (fn [w d x] (weight-update-i w 0.3 d x)) %1 delta-2 %2) w2 input2))))

(let [n (gen-weight net)
      input [0.4 0.4 0.4 0.4]
      target [0.1 0.2 0.3 0.4]]
  (list n
        (train-net-once n input target)))


