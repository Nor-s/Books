(define (square a) 
  (* a a))
(define (three-square a) 
  (* a a a))
(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))
  
(define (cube-root guess x)
  (if (good-enough? guess x)
       guess
      (cube-root (improve guess x)
                  x)))
  
(define (improve guess x)
  (/
   (+
     (/ x
        (square guess))
     (* 2 guess))
   3))
  
(define (good-enough? guess x)
  (< (abs (- (three-square guess) x)) 0.0000000001))
  
(define (three-sqrt x)
  (cube-root 1.0 x))
 