#lang racket
(require "utils.rkt")
(require "parser.rkt")
(require "directory.rkt")
(require "size.rkt")
(require "query.rkt")

(define run-order '("docs" "query" "size" "end"))

(define make-run
  (lambda (order env result)
    (case (car order)
      [("docs") (if (apply-env! env 'docs #t) (make-run
                                               (cdr order)
                                               env
                                               (cond
                                                 [(string? (apply-env! env 'docs #t)) (run-directory env)]
                                                 [else (make-run order (apply-env! env 'docs #t) result)]))
                    (make-run (cdr order) env result))]
      [("size") (if (apply-env! env 'size #t) (make-run (cdr order) env (run-size env result)) (make-run (cdr order) env result))]
      [("query") (if (apply-env! env 'query #t) (make-run (cdr order) env (run-query env result)) (make-run (cdr order) env result))]
      [else result])))

(define get-result
  (lambda (lst res)
    (if (empty? lst)
        res
        (get-result (cdr lst) (cons (car (car lst)) res)))))

(define run
  (lambda (path)
    (get-result (make-run run-order (init-env (file->string path)) '()) '())))
