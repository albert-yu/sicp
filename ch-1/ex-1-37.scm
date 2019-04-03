;; Ex. 1.37

(define (puts expr)
  (display expr (current-output-port))
  (newline (current-output-port)))


(define (cont-frac-iter n d k i)
  (if (= i k) 
    (/ (n i) (d i))
    (/ 
      (n i) 
      (+ (d i) (cont-frac-iter n d k (+ 1 i))))))
    


(define (cont-frac n d k)
  (cont-frac-iter n d k 1))


(puts (cont-frac (lambda (i) 1.0)
		 (lambda (i) 1.0)
		 1000))

