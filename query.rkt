#lang racket
(require "parser.rkt")
(require "utils.rkt")

; ------------------------------------------------------------------------------
; Helpers

(define array-equal?
  (lambda (arr1 arr2 th)
    (cond
      [(and (empty? arr1) (empty? arr2)) #t]
      [(or (empty? arr1) (empty? arr2)) #f]
      [else (and (lower-distance? (car arr1) (car arr2) th) (array-equal? (cdr arr1) (cdr arr2) th))])))

(define filter-text
(lambda (text-list res method query init th)
  (if (empty? text-list)
      res
      (if (method (cadr (car text-list)) (string-split query " ") init th)
          (filter-text (cdr text-list) (cons (car text-list) res) method query init th)
          (filter-text (cdr text-list) res method query init th)))))

(define levij
  (lambda (a b i j)
    (if (equal? (min i j) -1)
        (max (+ i 1) (+ j 1))
        (min (+ (levij a b (- i 1) j) 1) (+ (levij a b i (- j 1)) 1) (+ (levij a b (- i 1) (- j 1)) (if (equal? (string-ref a i) (string-ref b j)) 0 1))))))

(define distance
  (lambda (a b)
    (levij a b (- (string-length a) 1) (- (string-length b) 1))))

(define lower-distance?
  (lambda (a b th) (<= (distance a b) th)))

(define matcher
  (lambda (text method start end)
    (if (empty? text)
        #f
        (if (method (car text) start end)
            #t
            (matcher (cdr text) method start end)))))

; ------------------------------------------------------------------------------
; Exact queries

(define exact-query
  (lambda (text query res th)
    (if (empty? text)
        #f
        (if (array-equal? (head text (length query)) query th)
            #t
            (exact-query (cdr text) query res th)))))

(define and-query
  (lambda (text query res th)
    (if (empty? query)
        res
        (and res (and-query text (cdr query) (exact-query text (list (car query)) res th) th)))))

(define or-query
  (lambda (text query res th)
    (if (empty? query)
        res
        (or res (or-query text (cdr query) (exact-query text (list (car query)) res th) th)))))

; ------------------------------------------------------------------------------
; Wildcard queries

(define startswith
  (lambda (string start end)
    (string-prefix? string start)))

(define endswith
  (lambda (string start end)
    (string-suffix? string end)))

(define startsendswith
  (lambda (string start end)
    (and (startswith string start "") (endswith string "" end))))

(define strictstartsendswith
  (lambda (string start end)
    (and (startswith string start "") (endswith string "" end) (= (+ (string-length start) (string-length end)) (- (string-length string) 1)))))

(define regex-query
  (lambda (text regex res th)
    (cond
      [(string-suffix? (car regex) "*") (matcher text startswith (replace (car regex) '("*") "") "")]
      [(string-prefix? (car regex) "*") (matcher text endswith "" (replace (car regex) '("*") ""))]
      [(string-contains? (car regex) "*") (matcher text startsendswith (car (string-split (car regex) "*")) (cadr (string-split (car regex) "*")))]
      [else (matcher text strictstartsendswith (car (string-split (car regex) "?")) (cadr (string-split (car regex) "?")))])))

; ------------------------------------------------------------------------------
; Functions

(define is-func-query
  (lambda (query)
    (string-contains? query "(")))

(define func-data
  (lambda (query)
    (let ((splitted (string-split query "(")))
      (let ((name (car splitted)))
        (let ((inputs (string-split (string-trim (cadr splitted) ")") ",")))
          (cons name inputs))))))

(define init-func-env
  (lambda (args values env)
    (if (empty? args)
        env
        (init-func-env (cdr args) (cdr values) (extend-env (car args) (car values) env)))))

(define eval-query
  (lambda (body env)
    (let ((converted (map (lambda (x) (if (apply-env env (symbol->string x)) (apply-env env (symbol->string x)) (symbol->string x))) (cdr body))))
      (cond
        [(equal? (car body) '-)
         (let ((replaced (replace (car converted) (cdr converted) "")))
           (if (equal? replaced "") (list "-" (car converted)) (list "-" replaced)))]
        [(equal? (car body) '+) (list "+" (string-join converted))]
        [(equal? (car body) '*) (list "*" (string-join converted))]
        [(equal? (car body) 'exact) (list "exact" (string-join converted))]))))

(define run-func-query
  (lambda (env)
    (let ((query-data (func-data (apply-env env 'query))))
      (let ((name (car query-data)) (arg-vals (cdr query-data)))
        (let ((body (car (apply-env env name))) (args (cdr (apply-env env name))))
          (let ((func-env (init-func-env args arg-vals (empty-env))))
            (let ((evaluated (eval-query body func-env)))
              (cond
                [(equal? "-" (car evaluated))
                 (extend-env 'result (filter-text (apply-env env 'result) '() exact-query (cadr evaluated) #t (if (apply-env env 'distance) (apply-env env 'distance) 0)) env)]
                [(equal? "+" (car evaluated))
                 (extend-env 'result (filter-text (apply-env env 'result) '() or-query (cadr evaluated) #f (if (apply-env env 'distance) (apply-env env 'distance) 0)) env)]
                [(equal? "*" (car evaluated))
                 (extend-env 'result (filter-text (apply-env env 'result) '() and-query (cadr evaluated) #t (if (apply-env env 'distance) (apply-env env 'distance) 0)) env)]
                [(equal? "exact" (car evaluated))
                 (extend-env 'result (filter-text (apply-env env 'result) '() exact-query (cadr evaluated) #t (if (apply-env env 'distance) (apply-env env 'distance) 0)) env)]))))))))

; ------------------------------------------------------------------------------
; Run

(define run-query
  (lambda (env)
    (cond
      [(is-func-query (apply-env env 'query)) (run-func-query env)]
      [(apply-env env 'wildcard)
       (extend-env 'result (filter-text (apply-env env 'result) '() regex-query (apply-env env 'query) #t (if (apply-env env 'distance) (apply-env env 'distance) 0)) env)]
      [else
        (case (apply-env env 'method)
          [("exact") (extend-env 'result (filter-text (apply-env env 'result) '() exact-query (apply-env env 'query) #t (if (apply-env env 'distance) (apply-env env 'distance) 0)) env)]
          [("and") (extend-env 'result (filter-text (apply-env env 'result) '() and-query (apply-env env 'query) #t (if (apply-env env 'distance) (apply-env env 'distance) 0)) env)]
          [("or") (extend-env 'result (filter-text (apply-env env 'result) '() or-query (apply-env env 'query) #f (if (apply-env env 'distance) (apply-env env 'distance) 0)) env)]
          [else (extend-env 'result (filter-text (apply-env env 'result) '() exact-query (apply-env env 'query) #t (if (apply-env env 'distance) (apply-env env 'distance) 0)) env)])])))

(provide run-query)