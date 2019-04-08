;; Ex. 2.4

(define (puts expr)
  (display expr (current-output-port))
  (newline (current-output-port)))


(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))


;; test

(puts (car (cons 12 3)))



