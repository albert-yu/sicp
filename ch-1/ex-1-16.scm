;; Exercise 1.16

(define (square n) (* n n))


(define (even? n)
   (= (remainder n 2) 0))

(define (expt-iter b n state)
  (cond ((= 0 n) state)
        ((even? n) (expt-iter (square b) (/ n 2) state))
        (else (expt-iter b (- n 1) (* state b)))))


(define (power b n)
  (expt-iter b n 1))

;; test

(define result (power 10 5))

(display result (current-output-port))
(newline (current-output-port))

        
