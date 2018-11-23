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
        ((<= (weight x) (weight (car set))) (cons x set))
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
;ex2.67
(define (choose-branch bit branch)
   (cond ((= bit 0) (left-branch branch))
         ((= bit 1) (right-branch branch))
         (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (decode bits tree)
(define (decode-1 bits current-branch)
  (if (null? bits)
      '()
      (let ((next-branch
             (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
            (cons (symbol-leaf next-branch)
                  (decode-1 (cdr bits) tree))
            (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex2.68
(define (encode-symbol char tree)
  (define (char-of-symbol? char symbol)
    (cond ((null? symbol) false)
          ((equal? char (car symbol)) true)
          (else (char-of-symbol? char (cdr symbol)))))
  (define (encode-1 char current-branch)
    (if (leaf? current-branch)
        '()
        (if (char-of-symbol? char (symbols (left-branch current-branch)))
            (cons '0 (encode-1 char (left-branch current-branch)))
            (cons '1 (encode-1 char (right-branch current-branch))))))
  (if (char-of-symbol? char (symbols tree))
      (encode-1 char tree)
      (error "Not found" char)))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex2.69

(define (successive-merge s)
    (let ((merge (make-code-tree (car s) (cadr s))))
      (if (null? (cddr s))
         merge
         (let ((tree (successive-merge (adjoin-set merge (cddr s)))))
           tree))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex2.70
(define rock-huffman (list (list 'a 2)
                           (list 'na 16)
                           (list 'boom 1)
                           (list 'Sha 3)
                           (list 'Get 2)
                           (list 'yip 9)
                           (list 'job 2)
                           (list 'Wah 1)))
(define msg '(Get a job Sha na na na na na na na na
              Get a job Sha na na na na na na na na
              Wah yip yip yip yip yip yip yip yip yip
              Sha boom))
(define tree (generate-huffman-tree rock-huffman))
(encode msg tree)
;(1 1 1 1 0 1 1 0 1 1
; 1 1 1 1 1 1 1 0 0 0
; 0 0 0 0 0 0 1 1 1 1
; 0 1 1 0 1 1 1 1 1 1
; 1 1 1 0 0 0 0 0 0 0
; 0 0 1 1 0 0 1 1 0 1
; 0 1 0 1 0 1 0 1 0 1
; 0 1 0 1 0 1 1 1 0 1
;1 0 0 0)
;huffman => 84
;fixed   => 36*3 = 108