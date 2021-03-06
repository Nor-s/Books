#lang scheme

;2.61, 62 ....ordered list
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

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond 
             ((= x1 x2)          
              (cons x2
                    (union-set (cdr set1)
                               (cdr set2))))
             ((< x1 x2)
              (cons x1 (union-set (cdr set1) set2)))
             ((< x2 x1)
              (cons x2 (union-set set1 (cdr set2)))))))))
;2.63,64
;tree
(define (make-tree entry left right)
  (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

  (define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)                                  
      (let ((left-size (quotient (- n 1) 2)))                              
        (let ((left-result (partial-tree elts left-size)))     
          (let ((left-tree (car left-result))           
                (non-left-elts (cdr left-result))         
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
;2.65
  (define (union-tree tree1 tree2)
  (list->tree (union-set (tree->list-2 tree1)
                         (tree->list-2 tree2))))

(define (intersection-tree tree1 tree2)
  (list->tree (intersection-tree (tree->list-2 tree1)
                                 (tree->list-2 tree2))))
;;O(n) + O(n) + O(n) + O(n)
;
;
;
(define tree1 (make-tree 7
    (make-tree 3
               (make-tree 1 '() '())
               (make-tree 5 '() '()))
    (make-tree 9
               '()
               (make-tree 11 '() '()))))
(define tree2 (make-tree 3
    (make-tree 1 '() '())
    (make-tree 7
               (make-tree 5 '() '())
               (make-tree 9
                          '()
                          (make-tree 11 '() '())))))
(define tree3 (make-tree 4
    (make-tree 2 '() '())
    (make-tree 8
               (make-tree 6 '() '())
               (make-tree 10
                          '()
                          (make-tree 12 '() '())))))
;
(intersection-tree tree1 tree2)
(intersection-tree tree1 tree3)
(union-tree tree1 tree3)

;(5
;  (1 () (3 () ()))
;  (9 (7 () ()) (11 () ())))

;()

;(6
;  (3
;    (1 () (2 () ()))
;    (4 () (5 () ())))
;  (9
;    (7 () (8 () ()))
;    (11 (10 () ()) (12 () ()))))