#lang sicp
(#%require "../simply.rkt")
(#%require racket/trace)

(define (compose f g)
  (lambda (x) (f (g (x)))))

(define (roots a b c)
  ((lambda (d) (se (/ (+ (- b) d) (* 2 a)) (/ (- (- b) d) (* 2 a))))))
