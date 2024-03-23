#lang racket
(require 2htdp/batch-io
         threading)
(provide project-forward project-backward get-lines example-book example-line make-book-from-text example-book-left example-book-right combine-books new-lines make-book-from-file)
(struct spline (points))
(struct line (lhs rhs))
(struct point (data))

(struct book (raw splines lines points) #:transparent)

(define (example-book)
  (make-book-from-file "minimal-example.txt"))

(define (make-book-from-file filename)
  (make-book (read-file filename)))

(define (make-book-from-text t)
  (make-book t))

(define (get-sentences s)
  (map string-downcase (map string-trim (string-split s #px"\\.|\\?|\\;|\\!"))))

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

(define (project-forward book from)
  (remove-duplicates (map cadr (filter (λ (line) (equal? from (car line))) (book-lines book)))))

(define (project-backward book to)
  (remove-duplicates (map car (filter (λ (line) (equal? to (cadr line))) (book-lines book)))))

(define (get-lines book)
  (book-lines book))

(define (example-line)
  (list "a" "b"))

(define (example-book-left)
  (make-book-from-text "a b. c d. a c. b d. e f. g h."))

(define (example-book-right)
  (make-book-from-text "a b. c d. a c. b d. e g. f h."))

(define (combine-books left right)
  (book
   (string-append (book-raw left) "\n" (book-raw right))
   (remove-duplicates (append (book-splines left) (book-splines right)))
   (remove-duplicates (append (book-lines left) (book-lines right)))
   (remove-duplicates (append (book-points left) (book-points right)))))

(define (new-lines book1 book2)
  (set-symmetric-difference (book-lines book1) (book-lines book2)))

(define (example-combined-book) (combine-books (example-book-left) (example-book-right)))
