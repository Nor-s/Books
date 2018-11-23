#lang scheme

;ordered list
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

;n/2

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))
;O(n+m)...O(n)

;2.61
(define (adjoin-set x set)
    (cond ((null? set) (cons x set))        
          (else (let ((x1 (car set)))                 
                  (cond ((= x x1) set)
                        ((< x x1) (cons x set))
                        ((> x x1) (cons x1
                                       (adjoin-set x (cdr set)))))))))


;element-of-set? more faster -> adjoin-set more faster (both similiar)

(define x1 (list 1 3 5 7 9 10 11 12))

(adjoin-set 2 x1)
;(1 2 3 5 7 9 10 11 12)
(adjoin-set 4 (list 1 4))
;(1 4)
(adjoin-set 4 '())
;(4)