#lang racket
(require 2htdp/batch-io
         "folder.rkt"
         racket/serialize
         "registry.rkt")
(provide get-smallest-chunk
         hide-file
         delete
         (struct-out chunk)
         save-result
         ingest
         get-smallest-and-largest-result
         hide-result)

(define (load filename)
  (define registry-in (open-input-file filename))
  (define old-registry (deserialize (read registry-in)))
  (close-input-port registry-in)
  old-registry)

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
  (get-smallest-or-largest-chunk "db/chunks/" <))

(define (get-smallest-or-largest-chunk loc comparator)
  (if (empty? (directory-list loc))
      #f
      (letrec ([name
                (path->string
                 (car (car (sort
                            (map (λ (f) (cons f (file-size (string-append loc (path->string f)))))
                                 (directory-list loc))
                            (λ (f s) (comparator (cdr f) (cdr s)))))))]
               [contents (read-file (string-append loc name))])
        (chunk name contents))))

(define (get-smallest-or-largest-result loc comparator)
  (if (empty? (directory-list loc))
      #f
      (letrec ([name (path->string (car (car (get-filename-size-pairs-sorted loc comparator))))])
        (load (string-append loc name)))))

(define (get-filename-size-pairs-sorted loc comparator)
  (sort (map (λ (f) (cons f (file-size (string-append loc (path->string f))))) (directory-list loc))
        (λ (f s) (comparator (cdr f) (cdr s)))))

(define (get-smallest-result)
  (get-smallest-or-largest-result "db/results/" <))

(define (get-largest-result)
  (get-smallest-or-largest-result "db/results/" >))

(define (get-smallest-and-largest-result)
  (define s (get-smallest-result))
  (define l (get-largest-result))
  (if (or (not (and s l)) (equal? (result-filename s) (result-filename l))) #f (cons s l)))

(define (save-result r)
  (define res-out (open-output-file (string-append "db/results/" (result-filename r))))
  (write (serialize r) res-out)
  (close-output-port res-out))

(define (hide-file filename)
  (let ([new-name (string-append "db/processing/" filename)])
    (rename-file-or-directory (string-append "db/chunks/" filename) new-name)
    new-name))

(define (hide-result filename)
  (let ([new-name (string-append "db/processing-results/" filename)])
    (rename-file-or-directory (string-append "db/results/" filename) new-name)
    new-name))

(define (delete filename)
  (delete-file filename))
