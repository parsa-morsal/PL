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
    (levenshtein a b)))

(define (levenshtein a b)
  (define (ls0 a-index b-index)
    (cond [(or (= a-index -1) (= b-index -1)) (abs (- a-index b-index))]
          [else
           (define a-char (string-ref a a-index))
           (define b-char (string-ref b b-index))
           (if (equal? a-char b-char)
               (ls (sub1 a-index) (sub1 b-index))
               (min (add1 (ls (sub1 a-index) b-index))
                    (add1 (ls a-index (sub1 b-index)))
                    (add1 (ls (sub1 a-index) (sub1 b-index)))))]))
  (define memo (make-hash))
  (define (ls a-i b-i)
    (hash-ref! memo (cons a-i b-i) (λ() (ls0 a-i b-i))))
  (ls (sub1 (string-length a)) (sub1 (string-length b))))

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

(define get-args
  (lambda (str)
    (if (zero? (string-length str))
        '()
        (let ((first (car (regexp-match #rx"^[^,]*\\(.*\\)\\,?|^[^\\(\\),]*,?" str))))
          (cons (if (string-suffix? first ",") (substring first 0 (- (string-length first) 1)) first) (get-args (regexp-replace #rx"^[^,]*\\(.*\\)\\,?|^[^\\(\\),]*,?" str "")))))))

(define query-data
  (lambda (str)
    (let ((name (car (string-split (car (regexp-match #rx".*\\(.*\\)" str)) "("))))
      (let ((all-args (car (regexp-match #rx"\\(.*\\)" str))))
        (let ((clean-args (substring all-args 1 (- (string-length all-args) 1))))
          (let ((args (get-args clean-args)))
            (list name args)))))))

(define init-func-env
  (lambda (args values env)
    (if (empty? args)
        env
        (init-func-env (cdr args) (cdr values) (extend-env! (car args) (car values) env)))))

(define eval-query
  (lambda (body env result)
    (let ((initial-result
           (if (is-func-query (car body))
               (run-func-query env result (car body) #f)
               (filter-text result '() exact-query (car body) #t (if (apply-env! env 'distance #t) (apply-env! env 'distance #t) 0)))))
      (cond
        [(empty? (cdr body)) initial-result]
        [(and (equal? (cadr body) "*") (empty? initial-result)) initial-result]
        [(equal? (cadr body) "*") (eval-query (cddr body) env initial-result)]
        [else (remove-duplicates (append initial-result (eval-query (cddr body) env result)))]))))

(define run-func-query
  (lambda (env result query parent)
    (let ((call (query-data query)))
      (let ((name (car call)) (arg-vals (cadr call)))
        (let ((body (car (apply-env! env (string->symbol name) parent))) (args (cdr (apply-env! env (string->symbol name) parent))))
          (let ((func-env (init-func-env args arg-vals (empty-env! env))))
            (let ((converted (map (lambda (x)
                                    (cond
                                      [(apply-env! func-env (symbol->string x) parent) (apply-env! func-env (symbol->string x) parent)]
                                      [(apply-env! func-env x parent) (apply-env! func-env x parent)]
                                      [else (symbol->string x)])) body)))
              (begin
                (display "evaluating ")
                (display name)
                (newline)
                (eval-query converted func-env result)))))))))

; ------------------------------------------------------------------------------
; Run

(define run-query
  (lambda (env result)
    (cond
      [(is-func-query (apply-env! env 'query #t)) (run-func-query env result (apply-env! env 'query #t) #t)]
      [(apply-env! env 'wildcard #t)
       (filter-text result '() regex-query (apply-env! env 'query #t) #t (if (apply-env! env 'distance #t) (apply-env! env 'distance #t) 0))]
      [else
        (case (apply-env! env 'method #t)
          [("exact") (filter-text result '() exact-query (apply-env! env 'query #t) #t (if (apply-env! env 'distance #t) (apply-env! env 'distance #t) 0))]
          [("and") (filter-text result '() and-query (apply-env! env 'query #t) #t (if (apply-env! env 'distance #t) (apply-env! env 'distance #t) 0))]
          [("or") (filter-text result '() or-query (apply-env! env 'query #t) #f (if (apply-env! env 'distance #t) (apply-env! env 'distance #t) 0))]
          [else (filter-text result '() exact-query (apply-env! env 'query #t) #t (if (apply-env! env 'distance #t) (apply-env! env 'distance #t) 0))])])))

(provide run-query)