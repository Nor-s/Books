#lang planet neil/sicp

(define (equal? s1 s2)
  (cond ((and (null? s1) (null? s2)) #t)
        ((or (null? s1) (null? s2)) #f)
        ((and (pair? s1) (pair? s2))
         (and (equal? (car s1) (car s2))
              (equal? (cdr s1) (cdr s2))))
       ((eq? s1 s2) #t)
       (else #f)))
       
;ex2.60
;allow nested element 
;ex) (1 1 3 4 5 4)
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
; it's same but if false case -> step is more bigger
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (adjoin x set)
  (cons x set))
; allow nested element

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
;not allow nested element -> it's same
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (union-set set1 set2)
  (append set1 set2))
  ; allow nested element
(define (union-set-n s1 s2)
  (define (nest-remove set1 result)
    (if (null? set1)
        result
        (nest-remove (cdr set1) (adjoin (car set1) result))))
  (define (iter set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          (else (iter (cdr set1) (adjoin-set (car set1) set2)))))
  (iter s1 (nest-remove s2 '())))
;first procedure remove -> nested element of s2 
;                         (not remove nested element of s1 -> adjoin-set)
;second procedure same union-set3 of ex2-59.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))
;allow nested element it's same
(define (intersection-set-n set1 set2)
  (define (iter set result)
    (cond ((or (null? set) (null? set2)) result)
          ((e-of-set? (car set) set2)
                 (iter (cdr set) (adjoin-set (car set) result)))
          (else (iter (cdr set) result))))
  (iter set1 '()))
;impoltant: adjoin-set is check -> same-element?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define set-3 (list 1 2 2 1 4 9))
(define set-4 (list 2 3 6 7 9 3))

(union-set-n set-3 set-4)
;(9 2)
(intersection-set-n set-3 set-4)
;(4 1 9 7 6 3 2)

;efficiency compare (result: not allow nested)
;   element-of-set?: more element  -> The calculation step increases.
;                                  -> All Procedure slow down.
;                                  -> But stil O(n)
;   union-set-n: since nest-remove -> The calculation step increases.
;   intersction-set-n: Similar
;   adjoin-set: Same
;
;efficiency compare (result: allow nested)
;   element-of-set?: more element  -> The calculation step increases.
;                                  -> All Procedure slow down.
;                                  -> But stil O(n)
;   union-set: just append s1 & s2 -> O(n) more fast
;   adjoin: just (cons x set)      -> O(1) more fast
;   intersection-set: same

