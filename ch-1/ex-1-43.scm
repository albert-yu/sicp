;; Ex. 1.43


;; for printing out
(define (puts expr)
  (display expr (current-output-port))
  (newline (current-output-port)))

;; from Ex. 1.42
(define (compose f g)
  (lambda (x) (f (g x))))


(define (repeated f times)
  (if (= times 0)
    (lambda (x) x) ;; return identity func
    (compose f (repeated f (- times 1)))))


;; test

(define (sq x)
  (* x x))

(puts ((repeated sq 2) 5))


