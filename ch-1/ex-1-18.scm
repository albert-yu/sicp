;; Ex. 1.18

(define (double n)
  (+ n n))


;; floor division
(define (halve n)
  (if (even? n)
    (/ n 2)
    (/ (- n 1) 2)))


(define (mult-iter left right total)
  (cond ((= left 0) total)
        ((even? left) (mult-iter (halve left) (double right) total))
        (else (mult-iter (halve left) (double right) (+ total  right)))))

(define (peasant-mult a b)
  (mult-iter a b 0))


;; test

(display (peasant-mult 42398479283 5239480989023) (current-output-port))
(newline (current-output-port))



