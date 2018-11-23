;*applicative-order evaluation: 인자(p)에서 무한 루프 발생
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
(test 0 (p))        ;recursion

;*normal-order evaluation:
(define (p1) (p1))
(define (test1 0 (p1))
  (if (= 0 0)               ;first
      0                     ;return
      (p1)))
(test 0 (p1))

