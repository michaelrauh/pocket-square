#lang racket
(require 2htdp/batch-io "folder.rkt" racket/serialize "registry.rkt")
(provide get-smallest-chunk
         hide-file
         delete
         add-file (struct-out chunk) save-result ingest)
; todo fix hide file to work
; todo abstract files. Have it return a receipt and pass data if needed
; todo add ability to check out data in one go. Check out a chunk for example. This will hide it, and pass data with a receipt for later ack

(define (ingest filename)
  (add-file filename)
  (write-chunks filename))

(define (add-file filename)
  (rename-file-or-directory filename (string-append "db/" filename) #t))

(define (split-file filename)
  (string-split (read-file (string-append "db/" filename)) "CHAPTER"))

(define (write-chunks filename)
  (define chunks (split-file filename))
  (for ([chunk chunks] [i (in-naturals)])
    (write-file (string-append "db/chunks/" filename "-" (number->string i)) chunk))
  (delete-file (string-append "db/" filename)))

(struct chunk (name contents))

(define (get-smallest-chunk)
  (if (empty? (directory-list "db/chunks"))
      #f
      (letrec ([name 
        (path->string
         (car (car (sort
                    (map (λ (f) (cons f (file-size (string-append "db/chunks/" (path->string f)))))
                         (directory-list "db/chunks/"))
                    (λ (f s) (< (cdr f) (cdr s)))))))] [contents (read-file (string-append "db/chunks/" name))]) (chunk name contents))))

(define (save-result r)
  (define res-out (open-output-file (string-append "db/results/" (result-filename r) "-result")))
  (write (serialize r) res-out)
  (close-output-port res-out))

(define (hide-file filename)
  (displayln filename)
  (let ([new-name (string-append "db/processing/" filename)])
    (rename-file-or-directory (string-append "db/chunks/" filename) new-name)
    new-name))

(define (delete filename)
  (delete-file filename))
