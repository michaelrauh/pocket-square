#lang racket
(require "database.rkt")
(ingest (vector-ref (current-command-line-arguments) 0))