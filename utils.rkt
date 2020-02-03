#lang racket

(define head
  (lambda (lst k)
    (reverse (list-tail (reverse lst) (max (- (length lst) k) 0)))))

(define replace
  (lambda (str chars to)
    (if (empty? chars)
        str
        (replace (string-replace str (car chars) to) (cdr chars) to))))

(provide
  head
  replace)
