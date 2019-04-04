;; Ex. 1.39


;; for printing out
(define (puts expr)
  (display expr (current-output-port))
  (newline (current-output-port)))


;; continuous fraction from Ex. 1.37-------------------------------
(define (cont-frac-iter n d k i state)
  (cond ((= i 0) state)
	(else (cont-frac-iter n d k (- i 1) (/ 
				              (n i)
				              (+ (d i) state))))))


(define (cont-frac n d k)
  (cont-frac-iter n d k k 0))
;; ----------------------------------------------------------------


(define (tan-cf x k)
  (define (n i)
    (cond ((= i 1) x)
	  (else (- 0 (* x x)))))
  (define (d i)
    ;; get the i-th (1-based) odd number
    ;; 1, 3, 5, 7, etc.
    (+ 1 (* 2 (- i 1))))
  (cont-frac n d k))

      
(puts (exact->inexact (tan-cf 1 100)))

