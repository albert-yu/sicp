;; Ex. 1.19


(define (square n)
  (* n n))


;; Write T as a 2 x 2 matrix that acts on (a, b) such that
;; it results in the given transformation. 
;; This comes out to 
;; | p + q   q |
;; | q       p |

;; T^2 then gives
;; | (q + p)^2 + q^2    (q + p)*q + p*q |
;; | (q + p)*q + p*q    p^2 + q^2       |
;; yielding p' = p^2 + q^2
;; and q' = (q + p)*1 + p*q
;; you can verify that the upper left value is equal to 
;; p' + q'
(define (p_prime p q)
  (+ (square p) (square q)))


(define (q_prime p q)
  (+ (* p q) (* q (+ p q))))


(define (fib-iter a b p q count)
  (cond ((= count 0) a)
        ((even? count)
          (fib-iter a
                    b
                    (p_prime p q) 
                    (q_prime p q) 
                    (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1))))) 


(define (fib n)
  ;;        a b p q
  (fib-iter 1 0 0 1 n))


;; test

(define result (fib 100))

(display result (current-output-port))
(newline (current-output-port))

