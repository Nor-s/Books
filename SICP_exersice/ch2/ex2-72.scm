#lang scheme

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
;
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
;Procedure encode-symbol: O(N)
;Procedure encode: O(N^2)
;Suppose we have a Huffman tree for an alphabet of n symbols, and that the relative
;frequencies of the symbols are 1, 2, 4, ..., 2^n-1
;   N = all element = (1 + 2 + 4 + ... + 2^n-1)
;     = repeat of encode procedure
;   N = (about 2^n)
;   (1*2^(n-1) + ...+ (n-1)*2 + n*1) = step of encode-symbol
;   therefore
;    number of steps = N * (about 2^n)
;    number of steps  = N^2

