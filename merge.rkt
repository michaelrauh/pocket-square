#lang racket
(require "book.rkt" "square.rkt")
(provide new-squares)

(define (new-squares old current combined)
  (make-additional-both-ways combined (new-lines old current)))

(define (example-new-square) (new-squares (example-book-left) (example-book-right) (combine-books (example-book-left) (example-book-right))))