#lang racket
(require "book.rkt")
(provide make-all-squares make-squares-ffbb make-squares-fbbf)
(require racket/trace)

(define (make-ortho a b c d)
  ; (hash (hash) a (hash b 1) b (hash c 1) c (hash b 1 c 1) d)) ; todo comment this back in
  (list a b c d))

(define (make-all-squares book)
  (remove-duplicates (apply append (map (λ (a) (make-squares-ffbb book a)) (get-lines book)))))

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
