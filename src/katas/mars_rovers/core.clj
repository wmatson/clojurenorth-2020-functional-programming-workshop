(ns katas.mars-rovers.core)

(def ^:private direction-order [:N :E :S :W])

(defn- next-direction [order key]
  (->> (cycle order)
       (drop-while (fn [dir] (not= key dir)))
       (drop 1)
       first))

(def ^:private movement-vecs
  {:N [0 1]
   :S [0 -1]
   :E [1 0]
   :W [-1 0]})

(defmulti execute-command (fn dispatch [command plateau rover] command))

(defmethod execute-command :L [_ _ rover]
  (update-in rover [:position 2] #(next-direction (reverse direction-order) %)))

(defmethod execute-command :R [_ _ rover]
  (update-in rover [:position 2] #(next-direction direction-order %)))

(defn- clamp [bounds target]
  (map #(max 0 (min %1 %2)) bounds target))

(defmethod execute-command :M [_ plateau {:keys [position] :as rover}]
  (let [direction (last position)
        movement-vec (movement-vecs direction)
        new-pos (clamp plateau (map + position movement-vec))]
    (assoc rover
           :position
           (conj (vec new-pos) direction))))

(defn- step-rover [plateau rover]
  (let [next-instruction (first (:instructions rover))]
    (if next-instruction
      (-> (execute-command next-instruction plateau rover)
          (update :instructions rest))
      rover)))

(defn step [plateau rovers]
  (map #(step-rover plateau %) rovers))

(defn done? [rovers]
  (->> rovers
       (map :instructions)
       (map count)
       (apply +)
       (= 0)))

(defn go!
  [{:keys [plateau rovers]}]
  (loop [state rovers]
    (if-not (done? state)
      (recur (step plateau state))
      {:rovers
       (map #(select-keys % [:position]) state)})))

(comment
  
  (next-direction direction-order :N)
  
  (next-direction (reverse direction-order) :N)
  
  (go!
   {:plateau [5 5]
    :rovers [{:position [1 2 :N]
              :instructions [:M]}]})

  (go!
   {:plateau [5 5]
    :rovers [{:position [0 0 :N]
              :instructions [:M :L :M]}
             {:position [3 3 :W]}]}))