;; Ex. 1.17

(define (halve n)
  (/ n 2))

(define (double n)
  (+ n n))


(define (multiply-iter a b total)
  (cond ((= 0 b) total)
        ;; ((= 1 b) (+ total a))
        ;; ((= 2 b) (+ total (double a)))
        ((even? b) (multiply-iter a (halve b) (+ (double total) a)))
        (else (multiply-iter a (- b 1) (+ total a)))))


;; (define (multiply-fast a b)
;;   (multiply-iter a b 0))

(define (multiply-fast a b)
  (cond ((= 0 b) 0)
        ((even? b) (double (multiply-fast a (halve b))))
        (else (+ a (multiply-fast a (- b 1))))))

;; test

(display (multiply-fast 5 100000000000) (current-output-port))
(newline (current-output-port))


