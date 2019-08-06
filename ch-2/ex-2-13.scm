;; Ex. 2.13

(define (puts expr)
  (display expr (current-output-port))
  (newline (current-output-port)))

(define (put-interval x)
  ;; prints out a given interval
  (display "(" (current-output-port))
  (display (lower-bound x) (current-output-port))
  (display ", " (current-output-port))
  (display (upper-bound x) (current-output-port))
  (display ")" (current-output-port))
  (newline (current-output-port)))


(define (make-interval a b)
  (cons a b))


(define (upper-bound interval)
  (cdr interval))


(define (lower-bound interval)
  (car interval))


(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))


(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))


(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


;; definition of new constructor
;; with percent error tolerances
(define (make-center-percent c p)
  (let ((w (abs (* (/ p 100) c))))
    (make-center-width c w)))


;; definition of calculating the 
;; percentage that an interval 
;; spans
(define (percent i)
  (let ((c (abs (center i)))
        (w (width i)))
    (/ (/ w c) 100)))


(define (add-interval x y)
  (make-interval 
    (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))


(define (mul-interval-old x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


(define (pos? x)
  (>= x 0))


(define (pos-upp? intvl)
  ;; is the upper bound of this
  ;; interval positive?
  (pos? (upper-bound intvl)))


(define (pos-low? intvl)
  ;; is the lower bound of this 
  ;; interval positive?
  (pos? (lower-bound intvl)))


(define (neg-upp? intvl)
  (not (pos-upp? intvl)))


(define (neg-low? intvl)
  (not (pos-low? intvl)))


;; new definition of multiplying interval
(define (mul-interval x y)
  (cond
    ;; 1. all positive numbers
    ((and 
       (pos-upp? x)
       (pos-low? x)
       (pos-upp? y)
       (pos-low? y))
     (make-interval 
       (* (lower-bound x) (lower-bound y))
       (* (upper-bound x) (upper-bound y))))
    ;; 2. x's lower bound is negative
    ((and 
       (pos-upp? x)
       (neg-low? x)
       (pos-upp? y)
       (pos-low? y))
     (make-interval
       ;; upper-bound y times negative num will
       ;; be less than lower-bound y times negative
       ;; number
       (* (upper-bound y) (lower-bound x))
       (* (upper-bound x) (upper-bound y))))
    ;; 3. both lower bounds are negative
    ;; and both upper bounds are positive
    ((and 
       (neg-low? x)
       (pos-upp? x)
       (neg-low? y)
       (pos-upp? y))
     (make-interval
       (* (min (* (upper-bound x) (lower-bound y))
               (* (upper-bound y) (lower-bound x))))
       (* (upper-bound x) (upper-bound y))))
    ;; 4. x is all positive, y has neg lower bound
    ((and 
       (pos-low? x)
       (pos-upp? x)
       (neg-low? y)
       (neg-upp? y))
     (make-interval
       (* (lower-bound y) (upper-bound x))
       (* (upper-bound y) (upper-bound x))))
    ;; 5. x is all negative, y is all positive
    ((and 
       (neg-low? x)
       (neg-upp? x)
       (pos-low? y)
       (pos-upp? y))
     (make-interval
       (* (lower-bound x) (upper-bound y))
       (* (upper-bound x) (lower-bound y))))
    ;; 6. x is all neg, y's lower bound is neg
    ((and 
       (neg-low? x)
       (neg-upp? x)
       (neg-low? y)
       (pos-upp? y))
     (make-interval
       (* (upper-bound y) (lower-bound x))
       (* (lower-bound y) (lower-bound x))))
    ;; 7. same as 6, just x & y flipped
    ((and 
       (neg-low? x)
       (pos-upp? x)
       (neg-low? y)
       (neg-upp? y))
     (make-interval 
       (* (upper-bound x) (lower-bound y))
       (* (lower-bound x) (lower-bound y))))
    ;; 8. same as 5, just x & y flipped
    ((and 
       (pos-low? x)
       (pos-upp? x)
       (neg-low? y)
       (neg-upp? y))
     (make-interval
       (* (lower-bound y) (upper-bound x))
       (* (upper-bound y) (lower-bound x))))
    ;; 9. all negative
    ((and
       (neg-low? x)
       (neg-upp? x)
       (neg-low? y)
       (neg-upp? y))
     (make-interval
       (* (upper-bound x) (upper-bound y))
       (* (lower-bound x) (lower-bound y))))))


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


(define (percent-tol-prod percent-tol-x percent-tol-y)
  ;; approximate percentage of the tolerance of the product
  ;; in terms of the tolerances of the factors
  )

;; test
(let ((x (make-center-percent -6.8 10)))
  (put-interval x)
  (puts (percent x)))
