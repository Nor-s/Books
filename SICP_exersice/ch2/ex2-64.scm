#lang scheme

(define (make-tree entry left right)
  (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

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

;a.
;n is controll of median, amount of left elements, amount of right elements.
;The median of the list is the root of the tree, and
;medians of left and right elements are also the root of the subtree,
;and a subtree is created based on that value.
;This is done recursively to form a tree.
;
(list->tree (list 1 3 5 7 9))
;{5 {1 () 
;      {3 () ()}} 
;   {9 {7 () ()} 
;      {11 () ()}}}
;
;
;b.
;O(n)
;all-element -> only one time
;
;elts = (1 3 5 7 9)
;n = 5
;left-size  2
;left-result (partial-tree (1 3 5 7 9) 2)
;            ->n = 2, left-size = 0 ->left-result (partial-tree (1 3 5 7 9) 0)
;            left-result == (cons '() (1 3 5 7 9))
;            left-tree ==  '()
;            non-left-elts (1 3 5 7 9)
;            right-size    1
;            this-entry    1
;            right-result  (partial-tree (3 5 7 9) 1)
;                           n =1, left-size = 0 -> (cons '() (3 5 7 9))
;                           left-result ==(cons '() (3 5 7 9)
;                           left-tree =='()
;                           non-left-elts (3 5 7 9)
;                           right-size 0
;                           this-entry 3
;                           right-result (p-tr (5 7 9) 0) -> (cons '() (5 7 9))
;                           right-tree == '()
;                           remaining-elts (5 7 9)
;                           (cons make-tree (3 '() '()) (5 7 9))
;            right-result   ((3 '() '()) (5 7 9))
;            right-tree     (3 '() '())
;            remaining-elts (5 7 9)
;            (cons (make-tree 1 '() (3 '() '())) (5 7 9))
;left-result ((1 '() (3 '() '())) (5 7 9))
;
;recursion
;{
;    root = median (it's not work unorder list)
;    (... middle) -> left tree
;    (median ...) -> right tree
;    (cons (make-tree root left right) remaining)  
;}
