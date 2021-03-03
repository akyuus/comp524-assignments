#lang racket
(require (only-in (file "lex.rkt") lex))

(define tokens (make-parameter '()))

(define (check type)
  (if (empty? (tokens))
      #f
      (equal? type (first (first (tokens))))))

(define (consume type)
  (when (empty? (tokens))
    (error (~a "expected token of type " type " but no remaining tokens")))
  (let ([token (first (tokens))])
    (when (not (equal? type (first token)))
      (error (~a "expected token of type " type " but actual token was " token)))
    (tokens (rest (tokens)))  ; update tokens: remove first token
    token))

(define (parse code)
  (parameterize ([tokens (lex code)])
    (parse-program)))

;; program := exprList
(define (parse-program)
  (list 'program
        (parse-exprList)))

;; exprList := expr optExprList
(define (parse-exprList)
  (list 'exprList (parse-expr) (parse-optExprList)))

;; optExprList := ε | exprList
(define (parse-optExprList)
  (if (or (atom-pending?) (check 'OPAREN))
      (list 'optExprList (parse-exprList))
      (list 'optExprList)))

;; expr := atom | invocation
(define (parse-expr)
  (if (atom-pending?)
      (list 'expr (parse-atom))
      (list 'expr (parse-invocation))))

(define (atom-pending?)
  (or (check 'NAME) (check 'STRING) (number-pending?)))

(define (number-pending?)
  (or (check 'INT) (check 'FLOAT)))
            
(define (parse-atom)
  (if (number-pending?)
      (list 'atom (parse-number))
      (if (check 'NAME)
          (list 'atom (consume 'NAME))
          (list 'atom (consume 'STRING)))))

(define (parse-number)
  (if (check 'INT)
      (list 'number (consume 'INT))
      (list 'number (consume 'FLOAT))))

(define (parse-invocation)
  (list 'invocation (consume 'OPAREN) (parse-exprList) (consume 'CPAREN)))
