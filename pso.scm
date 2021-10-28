(use-modules (srfi srfi-1))

(define (vec-sub v1 v2 . ...)
  (map (lambda (xs) (apply - xs))
       (apply zip `(,v1 ,v2 ,@...))))

(define (vec-add v1 v2 . ...)
  (map (lambda (xs) (apply + xs))
       (apply zip `(,v1 ,v2 ,@...))))

(define (make-particle xlims ylims)
  (let* ((xlo (first xlims))
	 (xhi (second xlims))
	 (ylo (first ylims))
	 (yhi (second ylims))
	 (coords (list (+ xlo (random (- xhi xlo))) (+ ylo (random (- yhi ylo)))))
	 (velocity (list (* 0.1 (random 1.0)) (* 0.1 (random 1.0)))))
    (list (cons 'position coords)
	  (cons 'best coords)
	  (cons 'velocity velocity)
	  (cons 'history (list coords)))))

(define (make-swarm xlims ylims nparticles)
  (let lp [(i 0)
	   (res '())]
    (if (= i nparticles)
	res
	(lp (+ 1 i) (cons (make-particle xlims ylims) res)))))

(define (best-particle f swarm)
  (fold (lambda (p1 p2)
	  (if (< (apply f (assoc-ref p1 'position))
		 (apply f (assoc-ref p2 'position)))
	      p1 p2))
	(car swarm)
	(cdr swarm)))

(define (update-swarm swarm f cognition cohesion inertia)
  (let [(global-best (best-particle f swarm))]
    (define (update-particle p)
      (let* [(v (assoc-ref p 'velocity))
	     (xy (assoc-ref p 'position))
	     (b (assoc-ref p 'best))
	     (h (assoc-ref p 'history))
	     (new-v (vec-add (map (lambda (x) (* x inertia)) v)
			     (map (lambda (x) (* x cognition (random 1.0)))
				  (vec-sub b xy))
			     (map (lambda (x) (* x cohesion (random 1.0)))
				  (vec-sub (assoc-ref global-best 'position) xy))))
	     (new-xy (vec-add xy new-v))
	     (new-h (cons new-xy h))
	     (new-b (if (< (apply f new-xy) (apply f b)) new-xy b))]
	(list (cons 'velocity new-v)
	      (cons 'position new-xy)
	      (cons 'history new-h)
	      (cons 'best new-b))))
    (map (lambda (p) (update-particle p)) swarm)))

(define (mlmfunc x y)
  (+ (expt (- x 3.14) 2)
     (expt (- y 2.72) 2)
     (sin (+ (* 3 x) 1.41))
     (sin (- (* 4 y) 1.73))))

(define (ackley x y)
  (+ (- (* -20 (exp (* -0.2 (sqrt (* 0.5 (+ (expt (- x 1) 2) (expt (- y 1) 2)))))))
	(exp (* 0.5 (+ (cos (* 2 3.141592 x)) (cos (* 2 3.141592 y))))))
     (exp 1)
     20))

(define (pso f xlims ylims nparticles cognition cohesion inertia niter)
  (let lp [(i 0)
	   (swarm (make-swarm xlims ylims nparticles))]
    (if (= i niter)
	(assoc-ref (best-particle f swarm) 'best)
	(lp (+ i 1) (update-swarm swarm f cognition cohesion inertia)))))

(pso mlmfunc '(0 5.) '(0 5.) 500 0.01 0.05 0.5 100)
(pso mlmfunc '(0 5.) '(0 5.) 500 0.1 0.2 0.5 100)
(pso ackley '(-10 10.) '(-10 10.) 100 0.01 0.05 0.5 1000)


;; Writing data out to CSV

(define (pso-full-swarm f xlims ylims nparticles cognition cohesion w niter)
  (let lp [(i 0)
           (swarm (make-swarm xlims ylims nparticles))]
    (if (= i niter)
        swarm
        (lp (+ i 1) (update-swarm swarm f cognition cohesion w)))))

(define (create-output-data output)
  (let* [(histories (map (lambda (p) (reverse (assoc-ref p 'history))) output))
         (zipped (apply zip histories))
         (steps-data
          (map (lambda (step)
                 (map (lambda (xy) (cons (number->string step)
                                    (map number->string xy)))
                      (list-ref zipped step)))
               (iota (length zipped))))]
    (fold append (car steps-data) (cdr steps-data))))

(use-modules (csv csv))

(call-with-output-file "out.csv"
  (lambda (port)
    (sxml->csv 
     (create-output-data
      (pso-full-swarm ackley '(-10 10) '(-10 10) 20 0.01 0.05 0.5 50))
     port)))


;; Extracting repeat and iterate patterns

(define (repeatedly n f)
  (map (lambda (f) (f))
       (take (circular-list f) n)))

(define (repeatedly n f)
  (let lp [(i 0)
	   (res '())]
    (if (= i n)
	res
	(lp (+ 1 i) (cons (f) res)))))

(repeat 5 (lambda () (random 10)))

(define (iterate n f x)
  (let lp [(i 1)
	   (res (f x))]
    (if (= i n)
	res
	(lp (+ 1 i) (f res)))))

(iterate 3 (lambda (x) (* 2 x)) 1)

(define (make-swarm xlims ylims nparticles)
  (repeat nparticles (lambda () (make-particle xlims ylims))))

(define (pso f xlims ylims nparticles cognition cohesion inertia niter)
  (let?* [(s (make-swarm xlims ylims nparticles))
	  (s' (iterate niter (lambda (s) (update-swarm s f cognition cohesion inertia))))]
	 (assoc-ref (best-particle f updated-swarm) 'best)))
