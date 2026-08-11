#lang sicp
(#%require "../simply.rkt")

(define (plural wd)
  (if (equal? (last wd) 'y)
      (word (bl wd) 'ies)
      (word wd 's)))

;; Berkeley HW Week 1
(define (square x)
  (* x x))

(define (squares il)
  (if (null? il)
      '()
      (cons (square (car il)) (squares (cdr il)))))

(define (replace-word word)
  (cond
    [(or (equal? word 'I) (equal? word 'i)) 'you]
    [(equal? word 'me) 'you]
    [(equal? word 'you) 'me]
    [else word]))

(define (switch-rest sentence)
  (if (null? sentence)
      '()
      (cons (replace-word (car sentence)) (switch-rest (cdr sentence)))))

(define (switch sentence)
  (if (null? sentence)
      '()
      (cons (if (equal? (car sentence) 'you)
                'i
                (replace-word (car sentence)))
            (switch-rest (cdr sentence)))))
