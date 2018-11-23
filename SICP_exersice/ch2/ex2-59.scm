#lang planet neil/sicp

(define (equal? s1 s2)
  (cond ((and (null? s1) (null? s2)) #t)
        ((or (null? s1) (null? s2)) #f)
        ((and (pair? s1) (pair? s2))
         (and (equal? (car s1) (car s2))
              (equal? (cdr s1) (cdr s2))))
       ((eq? s1 s2) #t)
       (else #f)))
       
;ch2.3.3
;unordered list
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
;O(n)
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
;O(n)
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))
;O(n^2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex2.59.scm

(define (union-set1 set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union (cdr set1) set2))
        (else (union-set1 (cdr set1) (cons (car set1) set2)))))

; (not (element-of-set?) -> cons element set )== adjoin   
; and if set2 == '()
; step is O(n)

;recursive process
(define (union-set2 set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (adjoin-set (car set1)
                          (union-set2 (cdr set1) set2)))))
;iterative process
(define (union-set3 set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (union-set3 (cdr set1) (adjoin-set (car set1) set2)))))

; just (cdr set1) -> len set1: n && adjoin-set -> len set2: m
;-> O(n*m)  
(define set-1 (list 1 2 3 4 5 11))
(define set-2 (list 2 3 6 7 8))

(union-set3 set-1 set-2)
;(11 5 4 1 2 3 6 7 8)