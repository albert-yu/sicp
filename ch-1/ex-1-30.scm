;; Ex. 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))


;; test

(define (inc n)
  (+ 1 n))

(define (id n)
  n)

(define (puts thing)
  (display thing (current-output-port))
  (newline (current-output-port)))


(define res (sum id 1 inc 100))
(puts res) ;; 5050


