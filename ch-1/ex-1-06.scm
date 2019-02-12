(define (new-if pred then-clause else-clause)
  (cond (pred then-clause)
        (else else-clause)))


(new-if (= 2 3) 0 5)

(new-if (= 1 1) 0 5)

(define (square number)
  (* number number))


(square 9)

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.00001))

(define (average x y)
  (/ (+ x y) 2))


(define (improve guess x)
  (average guess (/ x guess)))


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (sqroot x)
  (sqrt-iter 1.0 x))


(sqroot 25)
