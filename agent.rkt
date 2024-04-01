#lang racket

(require "folder.rkt" "database.rkt" "book.rkt" "registry.rkt")
; todo make a binary merge agent
; todo make a linear merge agent
; note - adder.rkt starts it off
(define (single-fold-chunk c)
  (define secret-name (hide-file (chunk-name c)))
  (save-result (single-fold (result (chunk-name c) (make-book-from-text (chunk-contents c)) (make-registry))))
  (delete secret-name))

(define (single-fold-agent)
  (define c (get-smallest-chunk))
  (if (not c)
      (begin
        (sleep 10)
        (single-fold-agent))
      (begin
        (single-fold-chunk c)
        (single-fold-agent))))