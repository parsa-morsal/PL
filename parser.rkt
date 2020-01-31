#lang eopl
(require "utils.rkt")

; ------------------------------------------------------------------------------
; Scanner and parser specification

(define scanner-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (number (digit (arbno digit)) number)
    (identifier ((or letter "_" "," "/" "." "*" "?") (arbno (or letter digit "_" "," "/" "(" ")" "." "*" "?"))) symbol)))

(define grammar
  '((program ((arbno object)) a-program)
    (object ("{" (separated-list pair ",") "}") a-object)
    (pair ("\"" identifier "\"" ":" value) a-pair)
    (value (number) num-value)
    (value ("\"" identifier "\"") str-value)
    (value (list) lst-value)
    (value (object) obj-value)
    (list ("[" "\"" identifier "\"" (arbno operator "\"" identifier "\"") "]") a-list)
    (operator ("*") and-op)
    (operator ("+") or-op)
    (operator ("-") not-op)))

(sllgen:make-define-datatypes scanner-spec grammar)

(define list-the-datatypes
  (lambda()
    (sllgen:list-define-datatypes scanner-spec grammar)))

(define just-scan
  (sllgen:make-string-scanner scanner-spec grammar))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))

(provide
 scan&parse)

; ------------------------------------------------------------------------------
; Environments

(define empty-env
  (lambda ()
    (lambda (search-var)
      #f)))

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (lambda (search-var)
      (if (equal? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)))))

(define apply-env
  (lambda (env search-var)
    (env search-var)))

(provide
 empty-env
 extend-env
 apply-env)

; ------------------------------------------------------------------------------
; Initiating environments from program

(define lst->body
  (lambda (lst)
    (cases list lst
      (a-list (first ops rest)
        (if (empty? ops)
            (cons 'exact (cons first rest))
            (cases operator (car ops)
              (and-op () (cons '* (cons first rest)))
              (or-op () (cons '+ (cons first rest)))
              (not-op () (cons '- (cons first rest)))
              (else (cons 'exact (cons first rest)))))))))
    
(define val->prim
  (lambda (val)
    (cases value val
      (num-value (num) num)
      (str-value (str) (symbol->string str))
      (lst-value (lst) (lst->body lst))
      (obj-value (obj) obj))))

(define init-func-data
  (lambda (pairs data)
    (if (empty? pairs)
        (reverse data)
        (cases pair (car pairs)
          (a-pair (id val)
            (init-func-data (cdr pairs) (cons (val->prim val) data)))))))

(define init-pairs-env
  (lambda (pairs env)
    (if (empty? pairs)
        env
        (cases pair (car pairs)
          (a-pair (id val) (extend-env id (val->prim val) (init-pairs-env (cdr pairs) env)))))))

(define init-object-env
  (lambda (objs env)
    (if (empty? objs)
        env
        (cases object (car objs)
          (a-object (pairs)
            (cases pair (car pairs)
              (a-pair (id val)
                 (if (equal? id 'func)
                     (extend-env (val->prim val) (init-func-data (cdr pairs) '()) (init-object-env (cdr objs) env))
                     (init-object-env (cdr objs) (init-pairs-env pairs env))))))))))

(define init-program-env
  (lambda (pgm)
    (cases program pgm
      (a-program (objs) (init-object-env objs (empty-env))))))

(define init-env
  (lambda (str)
    (init-program-env (scan&parse str))))

(provide init-env)
 