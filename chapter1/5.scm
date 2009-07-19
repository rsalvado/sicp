; Exercise 1.5.  Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using applicative-order evaluation or normal-order evaluation. He defines the following two procedures:

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

; Then he evaluates the expression

; (test 0 (p))

; What behavior will Ben observe with an interpreter that uses applicative-order evaluation? What behavior will he observe with an interpreter that uses normal-order evaluation? Explain your answer. (Assume that the evaluation rule for the special form if is the same whether the interpreter is using normal or applicative order: The predicate expression is evaluated first, and the result determines whether to evaluate the consequent or the alternative expression.) 

; -> When using applicative-order evaluation we must first evaluate the subexpression (p) entering an infinite loop.
; -> When using normal-order evaluation we defer calculation until needed:
; -> (if (= 0 0) 0 (p)) this subexpression evalutes to zero and the test returns 0.