#lang racket
(require "square.rkt"
         "merge.rkt"
         "book.rkt"
         "registry.rkt"
         "folder.rkt")
(require racket/serialize
         racket/trace)

(define (bootstrap)
  (displayln "deleting databases.")
  (define bookshelf-out (open-output-file "bookshelf.bin" #:exists 'replace))
  (write (serialize (empty-book)) bookshelf-out)
  (close-output-port bookshelf-out)

  (define registry-out (open-output-file "registry.bin" #:exists 'replace))
  (write (serialize (make-registry)) registry-out)
  (close-output-port registry-out)
  (displayln "done."))

(define (bootstrap-binary)
  (bootstrap)
  (binary-list))

(define (binary-list)
  (display "input a list of filenames or quit: ")
  (define filenames (read-line (current-input-port) 'any))
  (time (binary-merge (map file-to-result (string-split filenames)))))

(define (bootstrap-run-list)
  (bootstrap)
  (run-list))

(define (run-list)
  (display "input a list of filenames or quit: ")
  (define filenames (read-line (current-input-port) 'any))
  (map (λ (filename) (time (run-with-filename filename))) (string-split filenames)))

(define (load filename)
  (define registry-in (open-input-file filename))
  (define old-registry (deserialize (read registry-in)))
  (close-input-port registry-in))

(define (save-temp data filename)
  (define data-out (open-output-file filename #:exists 'replace))
  (write (serialize data) data-out)
  (close-output-port data-out))

(define (overwrite)
  (displayln "overwriting previous database")
  (rename-file-or-directory "bookshelf-temp.bin" "bookshelf.bin" #t)
  (rename-file-or-directory "registry-temp.bin" "registry.bin" #t)
  (displayln "finished saving."))

(define (run-with-filename filename)
  (displayln "loading previous state.")
  (define old-registry (load "registry.bin"))
  (define old-book (load "bookshelf.bin"))
  (displayln (string-append "running filename: " filename))
  (define current-book (make-book-from-file filename))
  (define current-squares (make-all-squares current-book))
  (report current-squares old-registry)
  (define new-registry (registry-add old-registry current-squares))
  (define new-book (combine-books old-book current-book))
  (define merge-squares (new-squares old-book current-book new-book))
  (report-again new-registry merge-squares)
  (define merge-registry (registry-add new-registry merge-squares))
  (save-temp new-book "bookshelf-temp.bin")
  (save-temp merge-registry "registry-temp.bin")
  (overwrite))

(define (first-half l)
  (take l (floor (/ (length l) 2))))

(define (second-half l)
  (drop l (floor (/ (length l) 2))))

(define (binary-add numbers)
  (cond
    [(equal? (length numbers) 1) (car numbers)]
    [(equal? (length numbers) 2) (+ (car numbers) (cadr numbers))]
    [else (+ (binary-add (first-half numbers)) (binary-add (second-half numbers)))]))

(define (binary-merge results)
  (cond
    [(equal? (length results) 1) (car results)]
    [(equal? (length results) 2) (time (double-fold (car results) (cadr results)))]
    [else (double-fold (binary-merge (first-half results)) (binary-merge (second-half results)))]))

(define (file-to-result filename)
  (time (single-fold (result filename (make-book-from-file filename) (make-registry)))))

(bootstrap-binary)

;(bootstrap-run-list)
