
;new data -> 
;explicit dispatch, data-directed style, message-passing-style
;
;1. explicit dispatch
;   . adding a new type, a new op
;   . name don't nested
;   . each selector is defined to have a behavior that depends upon the particular type of data it is applied to.
;   . need know- all procedure: how work
;
;2. data-directed style
;   . add new type package: procedures in package-> 'put' hashmap - 2 step
;   . add new op: 'put' hashmap. pop-> just get  
;   
;3. message-passing-style
;   . add new type procedure: jest add - procedure: return dispatch - 1 step
;   . add new op: update dispatch procedure. and if it has many op -> many conditional stat in dispatch exe. 
;
;
; therefore: explicit dispatch << data-directed (many op) == message-passing-style (many type)
