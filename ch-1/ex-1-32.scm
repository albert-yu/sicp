;; Ex. 1.32 a & b

(define (accumulate-recur combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a) (accumulate-recur combiner null-value term (next a) next b))))


(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner result (term a)))))
  (iter a null-value))


;; test

(define (puts expr)
  (display expr (current-output-port))
  (newline (current-output-port)))


(define (id n)
  n)


(define (inc n)
  (+ 1 n))


(puts (accumulate-recur + 0 id 1 inc 100))

(puts (accumulate-iter + 0 id 1 inc 100))
