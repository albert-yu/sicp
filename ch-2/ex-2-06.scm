;; Ex. 2.6

(define (puts expr)
  (display expr (current-output-port))
  (newline (current-output-port)))


;; copied from book
(define zero (lambda (f) (lambda (x) x)))

;; also know as succ (successor)
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; definitions of one and two
(define one
  ;; f applied to x once
  (lambda (f) (lambda (x) (f x))))

(define two
  ;; f applied to x twice
  (lambda (f) (lambda (x) (f (f x)))))

;; define add as binary operator
(define (add num1 num2)
  ;; num1 and num2 are church numerals 
  (num1 (add-1 num2)))


(define to-int-iter
  (lambda (n) (+ 1 n)))

(define (to-integer church-num)
  (define (incr n)
    (+ 1 n))
  (define x 0)  
  (church-num (set! x (incr x)))
    x)

(define (num-foos church-num)
  (define (print-foo)
    (puts "start/")
    (puts "foo")
    (puts "/end"))
  (church-num print-foo))


;; test
(puts (add one two))
(puts (to-integer two))

