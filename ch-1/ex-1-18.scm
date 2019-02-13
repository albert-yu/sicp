;; Ex. 1.18

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))


(define (mult-iter a b counter total)
  (cond ((= b 0) 
           0)
        ((= counter b) 
           total)
        ((< (double counter) b)
           (mult-iter a b (double counter) (double total)))
        ((< counter b)
           (mult-iter a b (+ 1 counter) (+ a total)))))
          


(define (peasant-mult a b)
  (mult-iter a b 1 a))


;; test

(display (peasant-mult 100000 500000) (current-output-port))
(newline (current-output-port))



