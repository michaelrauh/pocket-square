#lang racket

(require "folder.rkt" "database.rkt" "book.rkt" "registry.rkt")

(define (single-fold-agent)
  (define c (get-smallest-chunk))
  (if (not c)
      (begin
        (displayln "sleeping")
        (sleep 10)
        (displayln "trying again")
        (single-fold-agent))
      (begin
        (single-fold-chunk c)
        (single-fold-agent))))

(define (single-fold-chunk c)
  (define secret-name (hide-file (chunk-name c)))
  (save-result (single-fold (result (chunk-name c) (make-book-from-text (chunk-contents c)) (make-registry))))
  (delete secret-name))

(single-fold-agent)