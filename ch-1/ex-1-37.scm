;; Ex. 1.37

(define (puts expr)
  (display expr (current-output-port))
  (newline (current-output-port)))

;; a)

(define (cont-frac-recur n d k i)
  (if (= i k) 
    (/ (n i) (d i))
    (/ 
      (n i) 
      (+ (d i) (cont-frac-recur n d k (+ 1 i))))))
    

(define (cont-frac n d k)
  (cont-frac-recur n d k 1))


(puts (cont-frac (lambda (i) 1.0)
		 (lambda (i) 1.0)
		 1000))


;; b)

(define (cont-frac-iter n d k i state)
  (if (= i k)
    (+
      state
      (/ (n i) (d i)))
    (cont-frac-iter n d k (+ i 1) (/ 
				    (n (+ i 1))
				    (+ (d (+ i 1)) state)))))


(define (cont-frac-iterative n d k)
  (cont-frac-iter n d k 1 0))


(puts (cont-frac-iterative (lambda (i) 1.0)
			   (lambda (i) 1.0)
			   1000))
