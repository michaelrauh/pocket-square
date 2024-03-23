#lang racket
(require "book.rkt" "square.rkt")
(provide new-squares)

(define (new-squares book1 book2 both)
  (make-additional-squares both (new-lines book1 book2)))

(define (example-new-square) (new-squares (example-book-left) (example-book-right) (combine-books (example-book-left) (example-book-right))))