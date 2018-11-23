#lang scheme

;put,get
;https://stackoverflow.com/questions/5499005
(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

;2.4 type
(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (attach-tag type-tag contents)
  (cons type-tag contents))

;ex2.73, ex2.58
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;sum
(define (install-sum-package)
  (define (sum? x)
    (and (pair? x) (eq? (car x) '+)))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (augend s) (car s))
  (define (addend s) (cadr s))
  (define (deriv-sum exp var)
        (make-sum (deriv (addend exp) var)
                  (deriv (augend exp) var)))

  (put 'deriv '+ deriv-sum)

  'done)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;product
(define (install-product-package)
  (define (product? x)
    (and (pair? x) (eq? (car x) '*)))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (multiplicand p) (car p))
  (define (multiplier p) (cadr p))

  (define (deriv-product exp var)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))

    (put 'deriv '* deriv-product)

  'done)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;expon
(define (install-exponentiation-package)
  (define (exponentiation? x)
    (and (pair? x) (eq? (car x) '**)))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (make-exponentiation b e)
    (cond ((=number? e 1) b)
          ((=number? e 0) 1)
          (else (list '** b e))))
  (define (base e) (car e))
  (define (exponent e) (cadr e))
  (define (deriv-expon exp var)
    (make-product
     (make-product (exponent exp)      
                   (make-exponentiation (base exp)
                                        (- (exponent exp) 1)))
     (deriv (base exp) var)))

  (put 'deriv '** deriv-expon)

  'done)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))
 
;a. number? and variable? work only one argument (not pair).
;   (sum?, product? -> pair)
;d. if use ((get (operator exp) 'deriv) (operands exp) var).
;   Swap 'op' and 'type' in 'put'.
;   ex) (put '** 'deriv deriv-expon)

(install-sum-package)
(install-product-package)
(install-exponentiation-package)

;c,d
(deriv '(* x (* y (+ x 3))) 'x)
;(+ (* y (+ x 3)) (* y x))
(deriv '(* (** x 3) 4) 'x)
;(* 4 (* 3 (** x 2)))