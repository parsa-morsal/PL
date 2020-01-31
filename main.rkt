#lang racket
(require "utils.rkt")
(require "parser.rkt")
(require "directory.rkt")
(require "size.rkt")
(require "query.rkt")

(define run-order '("directory" "query" "size" "end"))

(define make-run
  (lambda (order env)
    (case (car order)
      [("directory") (if (apply-env env 'directory) (make-run (cdr order) (run-directory env)) (make-run (cdr order) env))]
      [("size") (if (apply-env env 'size) (make-run (cdr order) (run-size env)) (make-run (cdr order) env))]
      [("query") (if (apply-env env 'query) (make-run (cdr order) (run-query env)) (make-run (cdr order) env))]
      [else env])))

(define get-result
  (lambda (lst res)
    (if (empty? lst)
        res
        (get-result (cdr lst) (cons (car (car lst)) res)))))

(define run
  (lambda (path)
    (get-result (apply-env (make-run run-order (init-env (file->string path))) 'result) '())))
