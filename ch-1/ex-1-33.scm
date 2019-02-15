;; Ex. 1.33


(define (filtered-accumulate combiner null-value term a next b filter-pred)
  (define (iter a result)
    (if (> a b)
      result
      (if (filter-pred a)
        (iter (next a) (combiner result (term a)))
        (iter (next a) result))))
  (iter a null-value))


;; Taken from section 1.2.6

(define (expmod base exponent m)
  (cond ((= exponent 0) 1)
        ((even? exponent)
          (remainder (square (expmod base (/ exponent 2) m)) 
                     m))
        (else
          (remainder (* base (expmod base (- exponent 1) m ))
                     m))))


(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))


(define (fast-prime? n times)
   (cond ((= times 0) true)
         ((fermat-test n) (fast-prime? n (- times 1)))
         (else false)))


(define (prime? n)
   (fast-prime? n 100))


;; test

;; sum of squares of prime numbers between 2 and 100

(define (puts expr)
  (display expr (current-output-port))
  (newline (current-output-port)))


(define (square x)
  (* x x))


(define (inc x)
  (+ 1 x))


(puts (filtered-accumulate + 0 square 2 inc 100 prime?)) 
