;; Ex. 1.42

;; for printing out
(define (puts expr)
  (display expr (current-output-port))
  (newline (current-output-port)))


(define (compose f g)
  (lambda (x) (f (g x))))


;; test

(define (sq x) (* x x))

(define (inc x) (+ 1 x))

(puts ((compose sq inc) 6))


