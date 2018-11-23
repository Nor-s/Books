#lang scheme

;2.2.1
(define (append list1 list2)
  (if  (null? list1)
       list2
       (cons (car list1) (append (cdr list1) list2))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;2.3.4
;Representing Huffman trees
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Sets of weighted elements
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))


(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex2.69
(define (successive-merge s)
    (let ((merge (make-code-tree (car s) (cadr s))))
      (if (null? (cddr s))
         merge
         (let ((tree (successive-merge (adjoin-set merge (cddr s)))))
           tree))))
;it's same ->
;(define (successive-merge s)
;  (if (null? (cddr s))
;      (make-code-tree (car s) (cadr s))
;      (successive-merge (adjoin-set (make-code-tree (car s) (cadr s))
;                                    (cddr s)))))
;it's same->
;(define (successive-merge s) 
;  (if (null? (cdr s)) 
;    (car s) 
;    (successive-merge 
;      (adjoin-set (make-code-tree (car s) (cadr s)) 
;                  (cddr s)))))
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;exe
(define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree
                     (make-leaf 'B 2)
                     (make-code-tree (make-leaf 'D 1)
                                     (make-leaf 'C 1)))))

(define sample-pairs(list (list 'A 4)
                          (list 'B 2)
                          (list 'C 1)
                          (list 'D 1)))

(newline)
sample-tree
;((leaf A 4)
; ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4)
; (A B D C)
; 8)
(newline)
(generate-huffman-tree sample-pairs)
;((leaf A 4)
; ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4)
; (A B D C)
; 8)