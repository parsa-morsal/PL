#lang racket
(require "parser.rkt")
(require "utils.rkt")

(define run-size
  (lambda (env result)
    (head result (apply-env! env 'size #t))))

(provide run-size)