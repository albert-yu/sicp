;; Ex. 1.17

(define (halve n)
  (/ n 2))

(define (double n)
  (+ n n))

(define (multiply-fast a b)
  (cond ((= 0 b) 0)
        ((even? b) (double (multiply-fast a (halve b))))
        (else (+ a (multiply-fast a (- b 1))))))

;; test

(display (multiply-fast 5 100000000000) (current-output-port))
(newline (current-output-port))


