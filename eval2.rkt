#lang racket

(require racket/sandbox
         2htdp/image
         racket/gui)
;(require wxme)

; replaces "#reader (lib \"<old>\"" with "#reader (lib \"<new>\""
; in the text of t (a text%) if it occurs
(define (replace-reader-lib t old new)
  (let ([to-find (format "#reader(lib \"~a\"" old)]
        [to-put (format "#reader(lib \"~a\"" new)])
    (send t move-position 'home)
    (match (send t find-string to-find)
      [(? number? i)
       (send t set-position i (+ i (string-length to-find)))
       (send t insert to-put)]
      [_ (void)]
      )))

(define HTDP-READERS
  '((beginner "htdp-beginner-reader.ss" (lib "htdp-beginner.ss" "lang"))
    (beginner-abbr "htdp-beginner-abbr-reader.ss" (lib "htdp-beginner-abbr.ss" "lang"))
    (intermediate "htdp-intermediate-reader.ss" (lib "htdp-intermediate.ss" "lang"))
    (intermediate-lambda "htdp-intermediate-lambda-reader.ss" 
                         (lib "htdp-intermediate-lambda.ss" "lang"))
    (advanced "htdp-advanced-reader.ss" (lib "htdp-advanced.ss" "lang"))))

; reader-for : symbol -> string
(define (reader-for lang)
  (cadr (assoc lang HTDP-READERS)))

; lang-for : symbol -> symbol
(define (lang-for lang)
  (caddr (assoc lang HTDP-READERS)))

; readers-below : symbol -> listof string
(define (readers-below lang)
  (map cadr (take HTDP-READERS
                  (sub1 (length (memf (λ(pair)
                                        (symbol=? lang (car pair)))
                                      HTDP-READERS))))))

; make-module-evaluator/submission
;             : bytes (#f or symbol or '(htdp ...)) -> #f or evaluator
; makes an evaluator for the program given in the data bytes
; if language is #f -- uses make-module-evaluator on the data
; if language is symbol -- uses make-evaluator with the specified language
; if (special beginner/beginner-abbr/intermediate/advanced)
;   then replaces reader header with the given level and then uses
;   make-module-evaluator
(define/contract (make-module-evaluator/submission data language)
  (-> bytes? (or/c #f symbol? (list/c 'htdp 
                                      (one-of/c 'beginner
                                                'beginner-abbr
                                                'intermediate
                                                'intermediate-lambda
                                                'advanced))) 
                   (or/c #f (any/c . -> . any)))
  (with-handlers ([exn? (λ (exn) (printf "eval2.rkt: ~a~n" exn) #f)])
    (parameterize ([sandbox-coverage-enabled #t]
                   [sandbox-path-permissions
                    '([write "/var/folders"]
                      [exists "/"]
                      [read "/"]
                      )]
                   [sandbox-namespace-specs
                    (list make-gui-namespace '2htdp/image)]
                   )
      (match language
        [#f (make-module-evaluator data)]
        [(? symbol? x) (make-module-evaluator data #:language x)]
        [(list 'htdp x)
         (let ([t (make-object text%)]
               [outp (open-output-bytes)])
           (send t insert-port (open-input-bytes data))
           (send t move-position 'home)
           (for/list ([r (readers-below x)])
             (replace-reader-lib t r (reader-for x)))
           (send t save-port outp)
           (let ([data-x (get-output-bytes outp)])
             (make-module-evaluator data-x #:language (lang-for x))))]))))

(provide make-module-evaluator/submission
         get-uncovered-expressions
         )
