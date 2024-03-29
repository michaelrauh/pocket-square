#lang racket
(provide make-registry registry-add get-net-new)
(require racket/trace)


(define (make-registry)
  (list))

(define (registry-add r os)
  (remove-duplicates (append r os)))

(define (get-net-new r os)
  ;(displayln os)
  ;(displayln "minus")
  ;(displayln r)
  ;(displayln "equals")
  ;(displayln (set-subtract os r))
  (set-subtract os r))

;(trace registry-add)