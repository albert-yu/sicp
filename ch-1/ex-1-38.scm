;; Ex. 1.38

;; for printing out
(define (puts expr)
  (display expr (current-output-port))
  (newline (current-output-port)))


;; continuous fraction from Ex. 1.37-------------------------------
(define (cont-frac-iter n d k i state)
  (cond ((= i 0) state)
	(else (cont-frac-iter n d k (- i 1) (/ 
				              (n i)
				              (+ (d i) state))))))


(define (cont-frac n d k)
  (cont-frac-iter n d k k 0))
;; ----------------------------------------------------------------


;; maps ith term to equivalent even number
(define (get-non-one-term i)
  (* 2 (+ (quotient (- i 1) 3) 1)))


;; gives ith term (1-based) of the sequence: 
;; 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8,...
(define (d-term j)
  (let ((i (- j 1)))

    (if (= (remainder i 3) 1) ;; is this the second term of every three elements? 
      (get-non-one-term i)
      1))) ;; else, just return 1


(define eulers-number (+ 2 (cont-frac (lambda (i) 1.0) d-term 1000)))


(puts eulers-number)


