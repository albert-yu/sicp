;; Ex. 1.29

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))


(define (inc n)
  (+ n 1))


(define (integral f a b n)
  (let ((h (/ (- b a) n)))
    (define (y-sub-k k)
      (f (+ a (* k h))))
    (define (t-sub-k k)
      (let ((y-k (y-sub-k k)))
        (cond ((or (= k 0) (= k n)) 
                y-k)
              ((even? k) 
                (* 2 y-k))
              (else 
                (* 4 y-k)))))
    (/ (* h (sum t-sub-k 0 inc n)) 3)))


;; test

(define (cube x)
  (* x x x))


(define ans (integral cube 0 1 100))

(define (puts item)
  (display item (current-input-port))
  (newline (current-input-port)))

(puts ans)

(define ans2 (integral cube 0 1 1000))

(puts ans2)
