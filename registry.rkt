#lang racket
(provide make-registry registry-add get-net-new)

(define (make-registry)
  (list))

(define (registry-add r os)
  (remove-duplicates (append r os)))

(define (get-net-new r os)
  (set-subtract os r))

(define ex1 (registry-add (make-registry) (list 1 2 3)))
(define ex2 (registry-add (make-registry) (list 2 3 4)))
(define example-difference (get-net-new ex1 ex2))