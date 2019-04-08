;; Ex. 2.5

(define (puts expr)
  (display expr (current-output-port))
  (newline (current-output-port)))


(define (cons-prime a b)
 (* (expt 2 a) (expt 3 b)))


(define (car-iter pair state)
  (if (= (modulo pair 2) 0)
    (car-iter (/ pair 2) (+ 1 state))
    state))


(define (car-prime pair)
  (car-iter pair 0))


(define (cdr-iter pair state)
  (if (= (modulo pair 3) 0)
    (cdr-iter (/ pair 3) (+ 1 state))
    state))


(define (cdr-prime pair)
  (cdr-iter pair 0))


(puts (car-prime (cons-prime 1 2)))


