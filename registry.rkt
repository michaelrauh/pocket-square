#lang racket
(provide make-registry registry-add get-net-new get-squares)
(require racket/trace)


(define (make-registry)
  (list))

(define (registry-add r os)
  (remove-duplicates (append r os)))

(define (get-net-new r os)
  (set-subtract os r))

(define (get-squares r)
  r)