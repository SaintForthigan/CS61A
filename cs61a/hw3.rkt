#lang sicp
(#%require "../simply.rkt")
(#%require racket/trace)

;; HW2

(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (inc a) b))))

(define (get-factors n)
  (filter (lambda (x) (zero? (remainder n x))) (enumerate-interval 1 (- n 1))))

(define (next-perf n)
  (let ([factors (get-factors n)])
    (if (= n (reduce + factors))
        n
        (next-perf (inc n)))))

;;SICP 1.16,35,37,38

;; 1.16
(define (square x)
  (* x x))

(define (fast-expt b n)
  (define (iter a b n)
    (cond
      [(= n 0) a]
      [(even? n) (iter a (square b) (/ n 2))]
      [else (iter (* a b) b (- n 1))]))
  (iter 1 b n))

;; 1.35
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ([next (f guess)])
      (if (close-enough? guess next)
          guess
          (try next))))
  (try first-guess))

;;(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1)
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2)

;; 1.37
(define (cont-frac n d k)
  (define (iter i)
    (cond
      [(= i k) (/ (n i) (d i))]
      [else (/ (n i) (+ (d i) (iter (inc i))))]))
  (iter 1))

(define (cont-frac-iter n d k)
  (define (iter i acc)
    (cond
      [(= i 0) acc]
      [else (iter (dec i) (/ (n i) (+ (d i) acc)))]))
  (iter k 0))

;; 1.38

(define (euler-approx i)
  (define (n k)
    1)
  (define (d k)
    (if (= 0 (remainder (+ k 1) 3))
        (* 2 (/ (+ k 1) 3))
        1))
  (+ 2.0 (cont-frac-iter n d i)))

(euler-approx 100)
