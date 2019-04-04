;; Ex. 2.1

;; for printing out
(define (puts expr)
  (display expr (current-output-port))
  (newline (current-output-port)))


;; greatest commmon divisor
;; for simplifying fractions
(define (gcd n d)
  (if (= b 0)
    a
    (gcd b (remainder a b))))


;; rational numbers
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0)
      ;; multiply top and bottom by -1 if
      ;; divisor is negative
      (cons (/ (* -1 n) g) (/ (* -1 d) g))
      (cons (/ n g) (/ d g))))) 

(define (numer x) 
  (car x))

(define (denom x) 
  (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y)))) 

(define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

(define (prnt-rat x)
  (display (numer x) (current-output-port))
  (display "/" (current-output-port))
  (display (denom x) (current-output-port))
  (newline (current-output-port)))



