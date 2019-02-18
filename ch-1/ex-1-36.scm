;; Ex. 1.35


(define (puts expr)
  (display expr (current-output-port))
  (newline (current-output-port)))


(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (puts guess) ;; print guess
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))


(fixed-point (lambda (x) (/ (log 1000) (log x))) 1.5)


