;; Ex. 2.1

;; for printing out
(define (puts expr)
  (display expr (current-output-port))
  (newline (current-output-port)))


;; greatest commmon divisor
;; for simplifying fractions
(define (gcd a b)
  (if (= 0 b)
    a
    (gcd b (remainder a b))))

(define (numer x) 
  (car x))

(define (denom x) 
  (cdr x))

(define (flip-signs x)
  (cons
    (* (numer x) -1)
    (* (denom x) -1)))

;; rational numbers
(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((raw (cons (/ n g) (/ d g))))
      (if (< (denom raw) 0)
        (flip-signs raw)
        raw))))


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


;; test
(prnt-rat (make-rat -1 -9))

(prnt-rat (make-rat 4 -2))



