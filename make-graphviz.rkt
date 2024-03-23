#lang racket

(require profile/sampler)
(require profile/analyzer)
(require profile/render-graphviz)
(require "run.rkt")


(define sampler (create-sampler (current-thread) 0.005))
(example-run)
(render (analyze-samples (sampler 'get-snapshots)))