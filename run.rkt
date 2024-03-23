#lang racket
(require "square.rkt" "merge.rkt" "book.rkt" "registry.rkt")
(require racket/serialize)
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


(define (sequential-stateful-run-bootstrap)
  (displayln "deleting databases.")
  (define bookshelf-out (open-output-file "bookshelf.bin" #:exists 'replace))
  (write (serialize (empty-book)) bookshelf-out)
  (close-output-port bookshelf-out)

  (define registry-out (open-output-file "registry.bin" #:exists 'replace))
  (write (serialize (make-registry)) registry-out)
  (close-output-port registry-out)
  (displayln "done.")
  
  (let loop ()
    (displayln "loading previous state.")
    (define bookshelf-in (open-input-file "bookshelf.bin"))
    (define old-book (deserialize (read bookshelf-in)))
    (close-input-port bookshelf-in)

    (define registry-in (open-input-file "registry.bin"))
    (define old-registry (deserialize (read registry-in)))
    (close-input-port registry-in)
    
    (display "input a filename or quit: ")
    (define filename (read-line (current-input-port) 'any))
    
    (define current-book (make-book-from-file filename))
    (displayln "finished reading file.")

    (displayln "finding new squares.")
    (define current-squares (make-all-squares current-book))

    (displayln "found this many: ")
    (displayln (length current-squares))
    
    (displayln "adding this many squares: ")
    (displayln (length (get-net-new old-registry current-squares)))

    (displayln "adding to registry.")
    (define new-registry (registry-add old-registry current-squares))
    
    (displayln "combining library.")
    (define new-book (combine-books old-book current-book))

    (displayln "calculating squares discovered via merge.")
    (define merge-squares (new-squares old-book current-book new-book))
    
    (displayln "discovered: ")
    (displayln (length (get-net-new new-registry merge-squares)))

    (displayln "updating registry with merge information.")
    (define merge-registry (registry-add new-registry merge-squares))
    
    (displayln "saving bookshelf.")
    (define bookshelf-out (open-output-file "bookshelf.bin" #:exists 'replace))
    (write (serialize new-book) bookshelf-out)
    (close-output-port bookshelf-out)

    (displayln "saving registry.")
    (define registry-out (open-output-file "registry.bin" #:exists 'replace))
    (write (serialize merge-registry) registry-out)
    (close-output-port registry-out)
    (displayln "finished saving.")
    
    (loop)))

(sequential-stateful-run-bootstrap)

