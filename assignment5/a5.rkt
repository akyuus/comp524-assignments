#lang racket

;; Grammar
;;
;; program     := exprList
;; exprList    := expr optExprList
;; optExprList := ɛ | exprList
;; expr        := atom | invocation
;; atom        := NAME | STRING | number
;; number      := INT | FLOAT
;; invocation  := OPAREN exprList CPAREN

(require (only-in (file "a3.rkt") parse))

(module+ test
  (require (only-in rackunit
                    check-equal?
                    check-exn
                    check-not-exn)))

(define (eval code)
  (eval-program (parse code)))

(define (eval-program program-expr)
  ;; program     := exprList
  (letrec ([exprList (second program-expr)])
    (last (eval-exprList exprList))))

(define (eval-exprList exprList)
  ;; exprList    := expr optExprList
  (letrec ([expr (second exprList)]
           [optExprList (third exprList)])
    (cons (eval-expr expr) (eval-optExprList optExprList))))

(define (eval-optExprList optExprList)
  ;; optExprList := ɛ | exprList
  (if (< (length optExprList) 2)
      null
      (eval-exprList (second optExprList))))

(define (eval-expr expr)
  ;; expr        := atom | invocation
  (letrec ([child (second expr)]
           [label (first child)])
    (case label
      [(atom) (eval-atom child)]
      [(invocation) (eval-invocation child)])))

(define (eval-atom atomExpr)
  ;; atom        := NAME | STRING | number
  (letrec ([child (second atomExpr)]
           [label (first child)])
    (case label
      [(NAME) (name-table (second child))]
      [(STRING) (second child)]
      [(number) (eval-number child)]
      [else (error "unrecognized label")])))

(define (name-table name)
  (case name
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]
    [(string-append) string-append]
    [(string<?) string<?]
    [(string=?) string=?]
    [(not) not]
    [(=) =]
    [(<) <]
    [else (error "unrecognized name")]))
     
(define (eval-invocation invocationExpr)
  ;; invocation  := OPAREN exprList CPAREN
  (letrec ([exprList (third invocationExpr)]
           [evaluated-expr (eval-exprList exprList)]
           [rator (first evaluated-expr)]
           [rands (rest evaluated-expr)])
    (apply rator rands)))

(define (eval-number numberExpr)
  (letrec ([number-token (second numberExpr)])
    (second number-token)))

(module+ test
  (check-equal? (eval "5") 5)
  (check-equal? (eval "(* 7 8)") 56)
  (check-equal? (eval "(* 7 (- 8))") -56)
  (check-equal? (eval "(* (+ 1 2) (/ 8 4))") 6)
  (check-equal? (eval "(string=? \"abc\" (string-append \"a\" \"b\" \"c\"))") #t)
  (check-equal? (eval "(string<? \"abc\" (string-append \"a\" \"b\" \"b\"))") #f)
  (check-equal? (eval "(not (string<? \"abc\" (string-append \"a\" \"b\" \"b\")))") #t)
  (check-exn exn:fail? (lambda () (eval "foo")))
  (check-exn exn:fail? (lambda () (eval "(list)")))
  (check-exn exn:fail? (lambda () (eval "(+ 1 \"a\")")))
  (check-exn exn:fail? (lambda () (eval "(string-append 1 \"a\")")))
  (check-exn exn:fail? (lambda () (eval "(/ 1 0)"))))
    