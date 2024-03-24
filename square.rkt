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
  (define ds (project-forward book b))
  (define all_cs (flatten (map (λ (d) (project-backward book d)) ds)))
  (define cs (filter (λ (c) (not (equal? b c))) all_cs))
  (define all_a_primes (flatten (map (λ (c) (project-backward book c)) cs)))
  (define a_primes (filter (λ (a_prime) (equal? a a_prime)) all_a_primes)) ; issue -> a must equal a prime or be filtered. Put another way, c must point to a or be filtered.
  
  (remove-duplicates (for*/list (
             [c cs]
             [d ds])
    (make-ortho a b c d))))

(define (make-squares-fbbf book line)
  (define b (car line))
  (define d (cadr line))
  (define all_cs (project-backward book b))
  (define cs (filter (λ (c) (not (equal? b c))) all_cs))
  (define as (flatten (map (λ (c) (project-backward book c)) cs)))
  (define all_b_primes (flatten (map (λ (a) (project-forward book a)) as)))
  (define b_primes (filter (λ (b_prime) (equal? b b_prime)) all_b_primes)) ; issue -> b must equal b prime or be filtered
  
  (remove-duplicates (for*/list (
             [c cs]
             [a as])
    (make-ortho a b c d))))

(define (example-square) (make-all-squares (example-book)))
(define (example-square-left) (make-all-squares (example-book-left)))
(define (example-square-right) (make-all-squares (example-book-right)))

; copy this approach to fix the above

;(define (smash word next prev)
;   (define d word)
;   (for*/set (
;               [c (hash-ref prev d (set))]
;               [a (hash-ref prev c (set))]
;               [b (hash-ref next a (set))]
;               [d-prime (hash-ref next b (set))]
;               #:when (and
;                       (equal? d d-prime)
;                       (not (equal? b c))))
;     (res a b c d)))