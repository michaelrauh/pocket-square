#lang racket
(require "book.rkt")
(provide make-additional-squares make-all-squares make-additional-both-ways)

(define (make-ortho a b c d)
  (hash (hash) a (hash b 1) b (hash c 1) c (hash b 1 c 1) d))

(define (make-all-squares book)
  (make-additional-squares book (get-lines book)))

(define (make-additional-squares book lines)
  (remove-duplicates (map (λ (a) (make-squares-ffbb book a)) lines)))

(define (make-additional-both-ways book lines)
  (remove-duplicates (append
                      (map (λ (a) (make-squares-ffbb book a)) lines)
                      (map (λ (a) (make-squares-fbbf book a)) lines)
                      ))) ; check for friends before calling each

(define (make-squares-ffbb book line)
  (define a (car line))
  (define b (cadr line))
  
  (remove-duplicates (for*/list (
             [d (project-forward book b)]
             [c (project-backward book d)]
             [a-prime (project-backward book c)]
             #:when (and
                       (equal? a a-prime)
                       (not (equal? b c))))
    (make-ortho a b c d))))

(define (make-squares-fbbf book line)
  (define b (car line))
  (define d (cadr line))

  (remove-duplicates (for*/list (
             [c (project-backward book d)]
             [a (project-backward book c)]
             [b-prime (project-forward book a)]
             #:when (and
                       (equal? b b-prime)
                       (not (equal? b c))))
    (make-ortho a b c d))))

(define (example-square) (make-all-squares (example-book)))
(define (example-square-left) (make-all-squares (example-book-left)))
(define (example-square-right) (make-all-squares (example-book-right)))