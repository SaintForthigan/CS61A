#lang sicp
(#%require "../simply.rkt")
(#%require racket/trace)

(define (pigl wd)
  (if (pl-done? wd)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

(define (pl-done? wd)
  (vowel? (first wd)))

(define (vowel? letter)
  (member? letter '(a e i o u)))

(define (square x)
  (* x x))

(define (sum-sq-3 a b c)
  (let* ([il (list a b c)]
         [sql (map square il)]
         [sqsum (apply + sql)])
    (- sqsum (square (apply min il)))))

(define (sum-sq a b c)
  (let ([sqsum (+ (square a) (square b) (square c))]) (- sqsum (square (min a b c)))))

(define (dupls-removed ise)
  (cond
    [(empty? ise) '()]
    [(member? (first ise) (bf ise)) (dupls-removed (bf ise))]
    [else (se (first ise) (dupls-removed (bf ise)))]))

(define (make-adder num)
  (lambda (x) (+ x num)))

(define (substitute ise owd nwd)
  (cond
    [(empty? ise) '()]
    [(equal? (first ise) owd) (se nwd (substitute (bf ise) owd nwd))]
    [else (se (first ise) (substitute (bf ise) owd nwd))]))

(substitute '(she loves you yeah yeah yeah) 'yeah 'maybe)

(define (t f)
  (lambda (x) (f (f (f x)))))

(define (s x)
  (+ 1 x))
