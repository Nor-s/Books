#lang scheme

(define (equal? s1 s2)
  (cond ((and (null? s1) (null? s2)) #t)
        ((or (null? s1) (null? s2)) #f)
        ((and (pair? s1) (pair? s2))
         (and (equal? (car s1) (car s2))
              (equal? (cdr s1) (cdr s2))))
       ((eq? s1 s2) #t)
       (else #f)))

(define (make-tree entry left right)
 (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

;2.66 Information retrieval - Tree
(define (make-records key . z)
  (list key z))
(define (key records)
  (car records))

(define (lookup given-key tree-of-records)
  (cond ((null? tree-of-records) false)
        ((equal? given-key (key (entry tree-of-records)))
         (entry tree-of-records))
        ((< given-key (key (entry tree-of-records)))
         (lookup given-key (left-branch tree-of-records)))
        ((> given-key (key (entry tree-of-records)))
         (lookup given-key (right-branch tree-of-records)))))

(define record1 (make-records 1 'k 'r))
(define record2 (make-records 2 'v 'r))
(define record3 (make-records 3 'g 'r))
(define record4 (make-records 4 'd 'r))
(define tree-records (make-tree record3
                                (make-tree record2
                                           (make-tree record1 '() '())
                                           '())
                                (make-tree record4
                                           '()
                                           '())))

(lookup 1 tree-records)
(lookup 3 tree-records)
(lookup 4 tree-records)
(lookup 2 tree-records)
(lookup 5 tree-records)
;(1 (k r))
;(3 (g r))
;(4 (d r))
;(2 (v r))
;#f