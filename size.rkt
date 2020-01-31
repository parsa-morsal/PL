#lang racket
(require "parser.rkt")
(require "utils.rkt")

(define run-size
  (lambda (env)
    (extend-env 'result (head (apply-env env 'result) (apply-env env 'size)) env)))

(provide run-size)