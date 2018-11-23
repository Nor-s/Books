;1.(sqrt 9)
;  sqrt 의 구현부->
;2.(sqrt-iter 1.0 x)
;  인자값 application,evaluation->
;3.(sqrt-iter 1.0 9)
;  a. x에 9
;  sqrt-iter의 구현부->
;4.(new-if (good-enough? guess x) guess (sqrt-iter (improve guess x) x))
;  인자값 application,evaluation->
;5.(new-if (good-enough? 1.0 9) 1.0 (sqrt-iter (improve 1.0 9) x))
;   a.(good-enough? 1.0 9) 실행
;   b.guess에 1.0
;   c.(sqrt-iter (improve 1.0 9) 9)  //-> improve 실행 ->2-3번 -> 무한루프

(define (square a) (* a a))

(define (abs x)
  (cond ((< x 0) (- x))
  (else x)))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)     
                  x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))