;; Ex. 2.10

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

;; floating equals
(define (fl-eq a b)
  (< (abs (- a b)) 0.00000001))


;; multiplicative inverse
(define (mul-inverse x)
  (let ((upper (upper-bound x))
        (lower (lower-bound x)))
    (if (fl-eq upper lower)
      (error "Cannot take multiplicative inverse of zero-length interval!")
      (make-interval (/ 1.0 upper)
                     (/ 1.0 lower)))))


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


(define (div-by-zero-test)
  (let ((a (make-interval 1.0 5.0))
        (b (make-interval 6.0 6.0)))
    (puts width (div-interval a b))))

(div-by-zero-test)

