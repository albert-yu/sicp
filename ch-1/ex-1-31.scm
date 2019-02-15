;; Ex. 1.31 a & b

;; iterative
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))
  (iter a 1))


;; recursive
(define (product-recur term a next b)
  (if (> a b)
    1
    (* (product-recur term (next a) next b) (term a))))


;; test

(define (id n)
  n)


(define (inc n)
  (+ 1 n))


(define (factorial-iter n)
  (product-iter id 1 inc n))


(define (factorial-recur n)
  (product-recur id 1 inc n))


(define (puts expr)
  (display expr (current-output-port))
  (newline (current-output-port)))


(puts (factorial-recur 10))

(puts (factorial-iter 10))
