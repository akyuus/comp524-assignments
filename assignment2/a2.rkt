#lang racket
(define punc-re #rx"^[(){},;.]")

(define int-re #rx"^-?[0-9]+(?=[\r\n\t (){},;.]|$)")

(define float-re #rx"^-?[0-9]*\\.[0-9]+(?=[\r\n\t (){},;.]|$)")

(define whitespace-re #rx"^[ \r\n\t]+")

(define comment-re #rx"^//[^\n]+\n|^//[^\n]+$|^/\\*.*\\*/")

(define string-re #rx"^\"[^\"]*\"(?=[\r\n\t (){},;.]|$)")

(define name-re #rx"^[^(){},;.\"0-9\r\n\t ][^(){},;.\"\r\n\t ]*(?=[\r\n\t (){},;.]|$)")

(define (tokenize type [data #f])
  (list type data))

(define (tokenize-name-or-keyword str)
  (case str
    [("def" "fun" "if" "not" "and" "or")
     (tokenize (string->symbol (string-upcase (string-trim str))))]
    [else (tokenize 'NAME (string->symbol str))]))

(define (tokenize-punctuation str)
  (tokenize
   (case str
     [("(") 'OPAREN]
     [(")") 'CPAREN]
     [("{") 'OBRACE]
     [("}") 'CBRACE]
     [(",") 'COMMA]
     [(";") 'SEMICOLON]
     [(".") 'PERIOD])))

(define (tokenize-int str)
  (tokenize
   'INT (string->number str)))

(define (tokenize-float str)
  (tokenize
   'FLOAT (string->number str)))

(define (tokenize-string str)
  (tokenize 'STRING (string-trim str "\"")))

(define (skip-match str)
  #f)

(define re-table
 (list
  (list punc-re tokenize-punctuation)
  (list int-re tokenize-int)
  (list float-re tokenize-float)
  (list comment-re skip-match)
  (list whitespace-re skip-match)
  (list string-re tokenize-string)
  (list name-re tokenize-name-or-keyword)))

;; check-re returns a sorted list of all matches against the current input token
(define (check-re str) ;test string against all re's
  (letrec ([matches (map (lambda (entry) (if (false? (first entry)) (list (list "") (second entry)) entry)) (map (lambda (entry) (list (regexp-match (first entry) str) (second entry))) re-table))]) ;unsorted list of matches
  (sort matches (lambda (m1 m2) (> (string-length (first (flatten m1))) (string-length (first (flatten m2))))))))

(define (lex str)
  (letrec ([helper (lambda (accumulator str)
    (letrec ([longest-match (flatten (first (check-re str)))])
    (if (non-empty-string? str)
      (if (non-empty-string? (first longest-match))
        (let ([final-token ((second longest-match) (first longest-match))])
          (if (false? final-token)
              (helper accumulator (substring str (string-length (first longest-match))))
          (helper (append accumulator (list final-token)) (substring str (string-length (first longest-match))))))
        (helper (append accumulator (list (list 'INVALID str))) (substring str (string-length str))))
      accumulator)))])
    (helper '() str)))