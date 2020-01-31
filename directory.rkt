#lang racket
(require racket/path)
(require "parser.rkt")
(require "utils.rkt")

(define replace-punctuations '("(" ")" "\"" "," "." "!" "?" ":" ";"))

(define scan
  (lambda (str)
    (string-split (replace str replace-punctuations "") " ")))

(define make-path-content
  (lambda (file-list string-list ext)
    (if (empty? file-list)
        string-list
        (if (equal? (path-get-extension (car file-list)) ext)
            (make-path-content (cdr file-list) (cons (list (file-name-from-path (car file-list)) (scan (file->string (simple-form-path (car file-list))))) string-list) ext)
            (make-path-content (cdr file-list) string-list ext)))))

(define path-content
  (lambda (path)
    (let ((dir (current-directory)))
      (begin
        (current-directory path)
        (let ((content (make-path-content (directory-list ".") '() #".txt")))
          (current-directory dir)
          content)))))

(define run-directory
  (lambda (env)
    (extend-env 'result (path-content (apply-env env 'directory)) env)))

(provide run-directory)