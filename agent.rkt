#lang racket

(require "folder.rkt" "database.rkt" "book.rkt" "registry.rkt")

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

(linear-agent)