;my blog: https://blog.naver.com/returns03/221310422793
;(define (good-enough? guess x)
;  (= (improve guess x) guess))
(define (square a) 
    (* a a))
  
(define (abs x)
    (cond ((< x 0) (- x))
    (else x)))
  
(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x)
                    x)))
  
(define (improve guess x)
    (average guess (/ x guess)))
  
(define (average x y)
    (/ (+ x y) 2))
  
(define (good-enough? guess x)
    (= (improve guess x) guess))
  
(define (sqrt x)
    (sqrt-iter 1.0 x))