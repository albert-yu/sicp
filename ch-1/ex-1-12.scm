;; Ex. 1.12

(define (print expr)
  (display expr (current-output-port)))

(define (puts expr)
  (print expr)
  (newline (current-output-port)))

; 1 0 0 0 0
; 1 1 0 0 0
; 1 2 1 0 0
; 1 3 3 1 0
; 1 4 6 4 1

(define (pascal-el row col)
  (if (or (= col 0) (= row col))
    1
   (if (> col row) 
     0
    (+
      (pascal-el (- row 1) col) 
      (pascal-el (- row 1) (- col 1))))))

(define (str-pascal-el row col)
  (if (not (> col row))
    (string-append (number->string (pascal-el row col)) " ")
   ""))

(define (str-row-iter row col)
  (if (= 0 col)
    (str-pascal-el 0 0)
   (string-append 
    (str-row-iter row (- col 1))
    " "
    (str-pascal-el row col))))

(define (str-row row)
  (string-append
   (str-row-iter row row)
   "\n"))

(define (str-pascal-iter row)
  (if (= 0 row)
    (str-row 0)
   (string-append
    (str-pascal-iter (- row 1))
    (str-row row))))

(define (show-pascal num-rows)
  (puts (str-pascal-iter (- num-rows 1))))

(show-pascal 10)
