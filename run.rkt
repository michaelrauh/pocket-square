#lang racket
(require "square.rkt" "merge.rkt" "book.rkt" "registry.rkt")
(provide example-run)
(define (example-run)
  (define r (make-registry))

  ;(define b1 (make-book-from-file "example.txt"))
  ;(define b2 (make-book-from-file "example2.txt"))

  (define b1 (make-book-from-text "a b. c d. a c. b d. e f. g h."))
  (define s1 (make-all-squares b1))
  (println (length (get-net-new r s1)))
  (define r1 (registry-add r s1))

  (define b2 (make-book-from-text "a b. c d. a c. b d. e g. f h."))
  (define s2 (make-all-squares b2))
  (println (length (get-net-new r1 s2)))
  (define r2 (registry-add r1 s2))
  
  (define b3 (combine-books b1 b2))
  (define s3 (new-squares b1 b2 b3))
  (println (length (get-net-new r2 s3)))
  (define r3 (registry-add r2 s3))
  
  'done)