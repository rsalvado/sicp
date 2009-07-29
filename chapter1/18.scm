;Exercise 1.18. Using the results of exercises 1.16 and 1.17, devise a procedure that generates an iterative
;process for multiplying two integers in terms of adding, doubling, and halving and uses a logarithmic
;number of steps.

(define (* a b)
  (define (even? n)
    (= (remainder n 2) 0))
  (define (halve n)
    (/ n 2))
  (define (double n)
    (+ n n))
  (define (iter a b n)
    (cond ((= b 0) n)
          ((even? b) (iter (double a) (halve b) n))
          (else (iter a (- b 1) (+ a n)))))
  (iter a b 0))