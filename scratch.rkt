#lang sicp
(#%require (rename sicp old-number->string number->string))
(define number->string (lambda (x) "hello"))
(display (number->string 42))
(newline)
