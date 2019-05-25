;; Ex. 2.9

(define (puts expr)
  (display expr (current-output-port))
  (newline (current-output-port)))


(define (make-interval a b)
  (cons a b))


(define (upper-bound interval)
  (cdr interval))


(define (lower-bound interval)
  (car interval))


;; implementation from book

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


;; multiplicative inverse
(define (mul-inverse x)
  (make-interval (/ 1.0 (upper-bound x))
                 (/ 1.0 (lower-bound x))))


(define (div-interval x y)
  (mul-interval
    x 
    (mul-inverse y)))


;; implement subtracting two intervals

;; define additive inverse

(define (add-inverse x)
  (make-interval (- 0 (upper-bound x))
                 (- 0 (lower-bound x))))


(define (sub-interval x y)
  (add-interval
    x
    (add-inverse y)))


;; define width
(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

;; f(a + b) == f(a) + f(b)
(define (sumtest)
  (let ((a (make-interval 1.0 5.0))
        (b (make-interval 3.0 6.0)))
    (puts (width (add-interval a b)))
    (puts (width a))
    (puts (width b))))


(sumtest)



