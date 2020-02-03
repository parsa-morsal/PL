#lang racket
(require (lib "eopl.ss" "eopl"))
(require "utils.rkt")

; ------------------------------------------------------------------------------
; Scanner and parser specification

(define scanner-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (number (digit (arbno digit)) number)
    (identifier ((or letter "_" "," "/" "." "*" "?") (arbno (or letter digit "_" "," "/" "(" ")" "." "*" "?"))) symbol)))

(define grammar
  '((program (object) a-program)
    (object ("{" (separated-list pair ",") "}") a-object)
    (pair ("\"" identifier "\"" ":" value) a-pair)
    (value (number) num-value)
    (value ("\"" identifier "\"") str-value)
    (value (func) lst-value)
    (value (object) obj-value)
    (func ("[" "\"" identifier "\"" (arbno operator "\"" identifier "\"") "]") a-list)
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

(define real-empty-env!
  (lambda ()
    (lambda (search-var find-here)
      #f)))

(define empty-env!
  (lambda (parent-env)
    (lambda (search-var find-here)
      (apply-env! parent-env search-var #t))))

(define extend-env!
  (lambda (saved-var saved-val saved-env)
    (lambda (search-var find-here)
      (if find-here
          (if (equal? search-var saved-var)
              saved-val
              (apply-env! saved-env search-var find-here))
          (apply-env! saved-env search-var find-here)))))

(define apply-env!
  (lambda (env var find-here)
    (env var find-here)))

(provide
 empty-env
 extend-env
 apply-env)

(provide
 real-empty-env!
 empty-env!
 extend-env!
 apply-env!)

; ------------------------------------------------------------------------------
; Initiating environments from program

(define keywords '(assignment directory docs query wildcard distance input body size))

(define lst->body
  (lambda (lst)
    (cases func lst
      (a-list (first ops rest)
        (let ((zip (map list ops rest)))
          (cons first (flatten (map (lambda (x)
                       (cases operator (car x)
                         (and-op () (list '* (cadr x)))
                         (or-op () (list '+ (cadr x)))
                         (not-op () (list '- (cadr x)))
                         (else (list 'exact (cadr x))))) zip))))))))

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
  (lambda (pairs is-assign env)
    (if (empty? pairs)
        env
        (cases pair (car pairs)
          (a-pair (id val)
                  (cond
                    [(equal? id 'assignment) (init-pairs-env (cdr pairs) #f (init-object-env (val->prim val) #t env))]
                    
                    [(equal? id 'docs)
                     (cond
                       [(string? (val->prim val)) (init-pairs-env (cdr pairs) #f (extend-env! id (val->prim val) env))]
                       [else (init-pairs-env (cdr pairs) #f (extend-env! id (init-object-env (val->prim val) #f (empty-env! env)) env))])]
                    
                    [(equal? id 'query)
                     (cond
                       [(apply-env! env (string->symbol (val->prim val)) #t) (init-pairs-env (cdr pairs) #f (extend-env! id (apply-env! env (string->symbol (val->prim val)) #t) env))]
                       [else (init-pairs-env (cdr pairs) #f (extend-env! id (val->prim val) env))])]
                    
                    [is-assign (init-pairs-env (cdr pairs) is-assign (extend-env! id (if (apply-env! env (string->symbol (val->prim val)) #f) (apply-env! env (string->symbol (val->prim val)) #f)
                                                                                         (val->prim val)) env))]
                    
                    [(index-of keywords id) (init-pairs-env (cdr pairs) is-assign (extend-env! id (val->prim val) env))]
                    
                    [else
                     (cases object (val->prim val)
                       (a-object (prs) (init-pairs-env (cdr pairs) #f (extend-env! id (init-func-data prs '()) env))))]))))))

(define init-object-env
  (lambda (obj is-assign env)
    (cases object obj
      (a-object (pairs)
        (init-pairs-env pairs is-assign env)))))

(define init-program-env
  (lambda (pgm)
    (cases program pgm
      (a-program (obj) (init-object-env obj #f (empty-env! (real-empty-env!)))))))

(define init-env
  (lambda (str)
    (init-program-env (scan&parse str))))

(provide init-env)
 