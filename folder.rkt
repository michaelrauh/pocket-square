#lang racket

(provide (struct-out result) report single-fold double-fold)
(require racket/serialize)
(serializable-struct result (filename bookshelf registry))
(require "square.rkt" "registry.rkt" "book.rkt" "square.rkt" "merge.rkt")
(define (single-fold r)
  (displayln (string-append "single running: " (result-filename r)))
  (define current-book (result-bookshelf r))
  (define current-squares (make-all-squares current-book))
  (define new-registry (registry-add (make-registry) current-squares))
  (report current-squares (make-registry))
  (result (result-filename r) current-book new-registry))

(define (report current-squares old-registry)
  (displayln (string-append "found this many: " (number->string (length current-squares)))))

(define (double-fold res1 res2)
  (displayln
   (string-append "double running: " (result-filename res1) " merging with " (result-filename res2)))

  (define old-registry (result-registry res1))
  (define old-book (result-bookshelf res1))

  (define current-book (result-bookshelf res2))
  (define current-registry (result-registry res2))

  (define current-squares (get-squares current-registry))
  (define old-squares (get-squares old-registry))

  (define new-registry (registry-add old-registry current-squares))
  (displayln (string-append "adding this many squares: "
                            (number->string (length (get-net-new old-registry current-squares)))
                            " out of a possible: "
                            (number->string (length current-squares))
                            " (" (number->string (* 100 (/ (length (get-net-new old-registry current-squares)) (exact->inexact (length current-squares))))) "%) " "to a registry with: " (number->string (length old-squares)) " squares preexisting"))
  (define new-book (combine-books old-book current-book))
  (displayln "merging...")
  (define merge-squares (new-squares old-book current-book new-book))
  (report-again new-registry merge-squares)
  (define merge-registry (registry-add new-registry merge-squares))
  (result (string-append (result-filename res1)) new-book merge-registry))

(define (report-again new-registry merge-squares)
  (displayln (string-append "discovered: "
                            (number->string (length (get-net-new new-registry merge-squares))))))