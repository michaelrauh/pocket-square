#lang racket
(require "parser.rkt")
(define (make-ortho a b c d)
  (hash (hash) "a" (hash "b" 1) "b" (hash "c" 1) "c" (hash "b" 1 "c" 1) "d"))

(define (make-all-squares book)
  (remove-duplicates (map (λ (a) (make-squares book a)) (get-lines book))))

(define (make-squares book line)
  (define a (car line))
  (define b (cadr line))
  (define ds (project-forward book a))
  (define all_cs (flatten (map (λ (d) (project-forward book d)) ds)))
  (define cs (filter (λ (c) (not (equal? b c))) all_cs))
  (define all_a_primes (flatten (map (λ (c) (project-backward book c)) cs)))
  (define a_primes (filter (λ (a_prime) (equal? a a_prime)) all_a_primes))
  (remove-duplicates (for*/list (
             [c cs]
             [d ds])
    (make-ortho a b c d))))

(define (example-square) (make-squares (example-book) (example-line)))

(example-square)