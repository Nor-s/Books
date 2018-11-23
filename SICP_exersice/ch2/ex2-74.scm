#lang scheme

;a
;division-name * employee-file * employee-name -> employee-record
(define (get-record division-name employee-file employee-name)
  ((get 'get-record division-name) employee-file employee-name))
;we know type information: 'get-record keyword
;it's find -> hashmap:'get-record and division-name
;          -> return: procedure:find a name in division file
;             It would probably be something like this:
;
(define (get-record-OOdivision employee-file employee-name)
  (cond ((null? employee-file) #f)
        ((equal? employee-name (get-name (car employee-file))) (car employee-file))
        (else (get-record-OOdivison (cdr employee-list) employee-name))))
;   file-structure: (list (list employee1) (list employee2) (3) ....) 
;   type information: name is string(symbol).
;                     and need procedure:Extract a name in division file
;   and this procedure -> put on map
(put 'get-record '(OOdivision) get-record-OOdivision)

;b
;divison-name * employee-record -> salary
(define (get-salary division-name employee-record)
  ((get 'get-salary division-name) employee-record))
;record struct: have element -> (list 'salary 10000) it's find easy salary inform as 'salary keyword.
;                            
;
(define (get-salary-OOdivision employee-record)
  (cond ((null? employee-file) (error "error not fond salary inform -- name:" (employee-name employee-record)))
        ((and (pair? (car employee-record))
              (equal? 'salary (car (car employee-record))))
         (car employee-record))
        (else (get-salary-OOdivison (cdr employee-record)))))
;    and this procedure -> put on map
(put 'get-salary '(OOdivision) get-salary-OOdivision)

;c
;all-file-list * employee-name -> employee-record
(define (find-employee-record all-file-list employee-name)
  (let (result (get-record (division-name (car all-file-list)) (car all-file-list) employee-name))
    (cond ((null? employee-file-list) (error "NOT FOUND -- name:" employee-name))
          ((not (pair? result)) (find-employee-record (cdr all-file-list) employee-name))   ;record is pair and #f is not pair
          (else result))))
;But it's not work - if exist person: The same name

;d
;just add hashmap -> put procedure.