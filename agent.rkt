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

(define (linear-fold-result l r)
  (define secret-name-l (hide-result (result-filename l)))
  (define secret-name-r (hide-result (result-filename r)))
  (save-result (double-fold r l))
  (delete secret-name-l)
  (delete secret-name-r))

(define (linear-agent)
  (define lr (get-smallest-and-largest-result))
  (if (not lr)
      (begin
        (displayln "sleeping")
        (sleep 10)
        (displayln "trying again")
        (linear-agent))
      (begin
        (linear-fold-result (car lr) (cdr lr))
        (linear-agent))))