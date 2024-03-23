#lang racket
(require 2htdp/batch-io
         threading)

(struct spline (points))
(struct line (lhs rhs))
(struct point (data))

(struct book (raw splines lines points) #:transparent)

;(define (input-from-file)
;  (input-strings (read-file "example.txt")))

(define s "this is a sentence? This is another. Here is an odd; one out!")

(define (get-sentences s)
  (map string-downcase (map string-trim (string-split s #px"\\.|\\?|\\;|\\!"))))

(define ex (car (get-sentences s)))

(define (heads s)
  (if (empty? s) s (cons s (heads (drop-right s 1)))))

(define (tails s)
  (if (empty? s) s (cons s (tails (drop s 1)))))

(define (splines s)
  (apply append (map tails (heads s))))

(define (split-sentence s)
  (string-split s))

(define (all-splines s)
  (remove-duplicates (apply append (map splines (map split-sentence (get-sentences s))))))

(define (get-all-splines s)
  (all-splines s))

(define (get-all-lines s)
  (filter (λ (x) (= (length x) 2)) (get-all-splines s)))

(define (get-all-points s)
  (sort (flatten (filter (λ (x) (= (length x) 1)) (get-all-splines s))) string<?))

(define (make-book s)
  (book s (get-all-splines s) (get-all-lines s) (get-all-points s)))
