#lang racket
(require "book.rkt" "square.rkt" racket/trace)
(provide new-squares)

(define (new-squares old current combined)
  (remove-duplicates (append
                      (apply append (for/list
                          ([line (new-lines old current)]
                           #:when (friends-do-not-all-match-forward old current line))
                        (make-squares-ffbb combined line)))
                      (apply append (for/list
                          ([line (new-lines old current)]
                           #:when (friends-do-not-all-match-backward old current line))
                        (make-squares-fbbf combined line)))
                      )))

(define (example-new-square) (new-squares (example-book-left) (example-book-right) (combine-books (example-book-left) (example-book-right))))