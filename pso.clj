(defn vec-sub [v1 v2 & rest]
  (let [xs `(~v1 ~v2 ~@rest)]
    (map #(apply - %) (apply map list xs))))

(defn vec-add [v1 v2 & rest]
  (let [xs `(~v1 ~v2 ~@rest)]
    (map #(apply + %) (apply map list xs))))

(defn make-particle [xlims ylims]
  (let [xlo (first xlims)
        xhi (second xlims)
        ylo (first ylims)
        yhi (second ylims)
        coords (list (+ xlo (rand (- xhi xlo))) (+ ylo (rand (- yhi ylo))))
        velocity (list (* 0.1 (rand)) (* 0.1 (rand)))]
    {:position coords
     :best coords
     :velocity velocity
     :history (list coords)}))

(defn make-swarm [xlims ylims nparticles]
  (repeatedly nparticles #(make-particle xlims ylims)))

(defn best-particle [f swarm]
  (first (sort-by #(apply f (:position %)) swarm)))

(defn update-swarm [swarm f cognition cohesion inertia]
  (let [global-best (best-particle f swarm)]
    (defn update-particle [p]
      (let [v (:velocity p)
            xy (:position p)
            b (:best p)
            h (:history p)
            new-v (vec-add (map #(* % inertia) v)
                           (map #(* % cognition (rand)) (vec-sub b xy))
                           (map #(* % cohesion (rand)) (vec-sub (:position global-best) xy)))
            new-xy (vec-add xy new-v)
            new-b (if (< (apply f new-xy) (apply f b)) new-xy b)
            new-h (cons new-xy h)]
        {:position new-xy
         :velocity new-v
         :best new-b
         :history new-h}))
    (map update-particle swarm)))

(defn mlmfunc [x y]
  (+ (Math/pow (- x 3.14) 2)
     (Math/pow (- y 2.72) 2)
     (Math/sin (+ (* 3 x) 1.41))
     (Math/sin (- (* 4 y) 1.73))))

(defn ackley [x y]
  (+ (-
      (* -20
         (Math/exp
          (* -0.2 (Math/sqrt (* 0.5 (+ (Math/pow (- x 1) 2) (Math/pow (- y 1) 2)))))))
      (Math/exp (* 0.5 (+ (Math/cos (* 2 Math/PI x)) (Math/cos (* 2 Math/PI y))))))
     (Math/exp 1)
     20))

(defn pso [f xlims ylims nparticles cognition cohesion inertia niter]
  (let [s (make-swarm xlims ylims nparticles)
        s' (last (take niter (iterate #(update-swarm % f cognition cohesion inertia) s)))]
    (:best (best-particle f s'))))

(pso mlmfunc '(0 5.) '(0 5.) 500 0.01 0.05 0.5 100)
(pso mlmfunc '(0 5.) '(0 5.) 500 0.1 0.2 0.5 100)
(pso ackley '(-10 10.) '(-10 10.) 100 0.01 0.05 0.5 200)


;; Write results out for animations

(defn pso-full-swarm [f xlims ylims nparticles cognition cohesion inertia niter]
  (let [s (make-swarm xlims ylims nparticles)]
    (last (take niter (iterate #(update-swarm % f cognition cohesion inertia) s)))))

(defn create-output-data
  [output]
  (let [histories (map #(reverse (:history %)) output)
        zipped (apply map list histories)
        steps-data (map
                    (fn [step] (map #(cons step %) (nth zipped step)))
                    (range (count zipped)))]
    (apply concat steps-data)))

(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(with-open [writer (io/writer "out-file.csv")]
  (csv/write-csv
   writer
   (create-output-data
    (pso-full-swarm ackley
                    '(-10 10.)
                    '(-10 10.) 3 0.01 0.05 0.5 10))))
