#lang racket

(provide (struct-out result) report single-fold)
(require racket/serialize)
(serializable-struct result (filename bookshelf registry))
(require "square.rkt" "registry.rkt")
(define (single-fold r)
  (displayln (string-append "single running: " (result-filename r)))
  (define current-book (result-bookshelf r))
  (define current-squares (make-all-squares current-book))
  (define new-registry (registry-add (make-registry) current-squares))
  (report current-squares (make-registry))
  (result (result-filename r) current-book new-registry))

(define (report current-squares old-registry)
  (displayln (string-append "found this many: " (number->string (length current-squares)))))