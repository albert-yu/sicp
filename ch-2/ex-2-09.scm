;; Ex. 2.9

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
  (/ (- (upper-interval x) (lower-interval x)) 2))
