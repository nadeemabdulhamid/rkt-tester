#lang racket

(require racket/async-channel)
(require "eval2.rkt" (only-in racket/sandbox kill-evaluator))


(define check?
  (or/c (list/c 'defined symbol?)
        (list/c 'proc symbol? number?)
        (list/c 'type any/c string? any/c) ; (-> any/c boolean?))
        (list/c 'test any/c)
        (list/c 'test any/c any/c)
        (list/c 'test any/c any/c any/c) ; (-> any/c any/c boolean?))
        (list/c 'test~ any/c number? number?) 
        ))

(define-struct/contract 
  problem ([name string?]
           [filename string?]
           [checks (listof check?)]
           ) #:transparent)

(define-struct/contract
  assignment ([name string?]
              [short-name string?]   ; should not have space
              [lang (or/c #f symbol? (list/c 'htdp        ;; matches what's in eval2.rkt
                                      (one-of/c 'beginner
                                                'beginner-abbr
                                                'intermediate
                                                'intermediate-lambda
                                                'advanced)))]
              [problems (listof problem?)]
              ) #:transparent)

; check : (list/c symbol any ...)
; examples:
;   `(defined <id:symbol>)          ... if the symbol is defined at all
;   `(proc <id:symbol> <number>)    ... if the symbol is defined as a procedure with given arity
;   `(type <expr:sexp> <string> ,<pred:_ -> boolean>)
;   `(test <expr:sexp>)
;   `(test <expr:sexp> ,<value:any>)
;   `(test <expr:sexp> ,<value:any> ,<equal?:_ _ -> boolean>)
;   `(test~ <exp:sexp> ,<value:number> ,<delta:number>)

#| template:
  (match chk 
           [`(defined ,sym)  ... ]
           [`(proc ,sym ,num)  ... ]
           [`(type ,exp ,descrip ,pred) ... ]
           [`(test ,exp) ... ]
           [`(test ,e1 ,e2) ... ]
           [`(test ,e1 ,e2 ,comp) ... ]
           [`(test~ ,e1 ,n ,d) ... ]
   ))
|#

(define-struct/contract
  problem-report ([name string?]
                  [filename string?]
                  [evaluated? boolean?]
                  [timeout? boolean?]
                  [filename-good? boolean?]
                  [coverage-good? boolean?]
                  [results (listof (cons/c boolean? check?))]
                  ) #:transparent)

(define-struct/contract
  assignment-report ([name string?]
                     [short-name string?]   ; should not have space
                     [lang (or/c #f symbol? (list/c 'htdp        ;; matches what's in eval2.rkt
                                      (one-of/c 'beginner
                                                'beginner-abbr
                                                'intermediate
                                                'intermediate-lambda
                                                'advanced)))]
                     [problems (listof problem-report?)]
                     ) #:transparent)
                     


;;=================================================================================

(define true? (compose not not))

(define problem-check-timeout (make-parameter 15))
; problem-check-timeout --- parameter controlling how long each problem check is given to run in 
;   a separate thread

; check/assignment : assignment? (listof ((filename . bytes?) or #f)) -> assignment-report?
; matches up the filenames with the problems in the assignment (the two lists
; must be the same length), attempts to make evaluators for each file and apply
; check-problem, collecting the results in the assignment-report.
(define (check/assignment the-a submits)
  (match the-a
    [(struct assignment (name short-name lang (list probs ...)))
     (assignment-report name short-name lang
                        (map (λ(p s) 
                               (match s 
                                 [(cons fn bs)
                                  (printf "FILE: ~a~n" fn)
                                  (let* ([eval (run-with-timeout 
                                                (λ() (make-module-evaluator/submission bs lang))
                                                (problem-check-timeout)
                                                (λ() #f))]
                                         [result 
                                          (run-with-timeout
                                           (λ() (check/problem p eval fn))
                                           (problem-check-timeout)
                                           (λ() (fail-timeout/problem p eval fn)))])
                                    (and eval (kill-evaluator eval))
                                    result)
                                  ]
                                 [_ (fail-all/problem p)]))
                             probs submits))
     ]))

; run-with-timeout : ( -> any) number ( -> any) -> any
(define (run-with-timeout thunk timeout failure-thunk)
  (let* ([mailbox (make-async-channel)]
         [T (thread (λ() (async-channel-put mailbox (thunk))))])
    (or (sync/timeout timeout mailbox)
        (failure-thunk))))

; check/problem : problem? (evaluator? or #f) (filename? or #f) -> problem-report?
; produces a problem-report based on the input
; if the evaluator is #f the checked? flag of the problem-report will be #f
(define (check/problem prob eval filename/submit)
  (match prob
    [(struct problem (name filename (list chks ...)))
     ;(when (get-uncovered-expressions eval #f)
     ;  (map
     ;   (λ(s)
     ;     (display (syntax-source-module s #t)) (newline))
     ;   (get-uncovered-expressions eval)))
     (problem-report name
                     filename
                     (true? eval)
                     #f
                     (and (string? filename/submit) (string=? filename filename/submit))
                     (and eval (empty? (get-uncovered-expressions eval)))
                     (map (λ(c) (run-check c eval)) chks))
     ]))

; match-exactness : any any -> any
; coerces the first number to match the exactness nature of the second
;   if the arguments are not numbers, the first argument is produced, unchanged
(define (match-exactness v n)
  (if (not (and (number? v) (number? n)))
      v
      (cond
        [(inexact? n) (if (inexact? v) v (exact->inexact v))]
        [(exact? n) (if (exact? v) v (inexact->exact v))])))


; run-check : check? (evaluator? or #f) -> (cons/c boolean? check?)
(define (run-check chk eval)
  (cons
   (with-handlers ([exn? (λ (exn) #f)])
     (if (not eval) #f
         (match chk 
           [`(defined ,sym) (eval sym) #t]
           [`(proc ,sym ,num) (let ([arity (procedure-arity (eval sym))])
                                (= arity num))]
           [`(type ,exp ,descrip ,pred) (true? (pred (eval exp)))]
           [`(test ,exp) (true? (eval exp))]
           [`(test ,e1 ,e2) (equal? (match-exactness (eval e1) e2) e2)]
           [`(test ,e1 ,e2 ,comp) (true? (comp (eval e1) e2))]
           [`(test~ ,e1 ,n ,d) (<= (- n d) (eval e1) (+ n d))]
           )))
   chk))

; fail-all/problem : problem? -> problem-report?
(define (fail-all/problem prob)
  (match prob
    [(struct problem (name filename (list chks ...)))
     (problem-report name
                     filename
                     #f
                     #f
                     #f
                     #f
                     (map (λ(c) (cons #f c)) chks))]))

; fail-timeout/problem : problem? (evaluator or #f) (string or #f) -> problem-report?
(define (fail-timeout/problem prob eval filename/submit)
  (match prob
    [(struct problem (name filename (list chks ...)))
     (problem-report name
                     filename
                     (true? eval)
                     #t
                     (and (string? filename/submit) (string=? filename filename/submit))
                     (and eval (empty? (get-uncovered-expressions eval)))
                     (map (λ(c) (cons #f c)) chks))]))


;;=================================================================================

;; statistic functions

;; I'VE TEMPORARILY DISABLED COVERAGE REPORTING/STATS

(define (count-total-checks/problem pr)
  (+ 3   ; eval, timeout, coverage, filename checks
     (length (problem-report-results pr))))

(define (count-failed-checks/problem pr)
  (+ (if (problem-report-evaluated? pr) 0 1)
     (if (and (problem-report-evaluated? pr) (not (problem-report-timeout? pr))) 0 1)
     (if (problem-report-filename-good? pr) 0 1)
;;     (if (problem-report-coverage-good? pr) 0 1)
     (apply + (map (λ(chk) (if (car chk) 0 1)) (problem-report-results pr)))))

(define (count-passed-checks/problem pr)
  (- (count-total-checks/problem pr) (count-failed-checks/problem pr)))

(define (count-total-checks/assignment ar)
  (apply + (map count-total-checks/problem (assignment-report-problems ar))))

(define (count-failed-checks/assignment ar)
  (apply + (map count-failed-checks/problem (assignment-report-problems ar))))

(define (count-passed-checks/assignment ar)
  (- (count-total-checks/assignment ar) (count-failed-checks/assignment ar)))
  

;;=================================================================================

;; text rendering functions


(define (render-check/txt chk)
  (match chk 
           [`(defined ,sym)  (format "Is '~s' defined?" sym)]
           [`(proc ,sym ,num) (format "Is '~s' defined as a function of ~s parameter~a?" sym num
                                      (if (= 1 num) "" "s"))]
           [`(type ,exp ,descrip ,pred) (format "Does ~s produce ~a?" exp descrip)]
           [`(test ,exp) (format "Does ~s produce a result other than 'false'?" exp)]
           [`(test ,e1 ,e2) (format "Does ~s produce an expected result?" e1)]
           [`(test ,e1 ,e2 ,comp) (format "Does ~s produce an expected result?" e1)]
           [`(test~ ,e1 ,n ,d) (format "Does ~s produce ~a +/- ~a?" e1 n d)]
    ))

(define (render-check-result/txt chk/r)
  (string-append
   (render/pf (car chk/r))
   (render-check/txt (cdr chk/r))
   "\n"))

(define (render/pf v) 
  (if v "   PASS: " " x FAIL: "))

(define (render-problem-report/txt pr)
  (match pr
    [(struct problem-report (name filename evald? timeout? fnamegood? covered? (list chks ...)))
     
     (string-append
      (format "PROBLEM: ~a\n" name)
      (format " Passed ~a out of ~a tests.\n" (count-passed-checks/problem pr)
              (count-total-checks/problem pr))
      (render/pf fnamegood?) (format "File name matches '~a'?\n" filename)
      (render/pf evald?) "File evaluated without error?\n"
      (render/pf (and evald? (not timeout?))) "File ran without timeout?\n"
;;      (render/pf covered?) "Tests cover all expressions?\n"
      "   ----\n"
      (if (and fnamegood? evald? (not timeout?))
          (apply string-append (map render-check-result/txt chks))
          "   <<<... other tests fail ...>>>\n")
      )
     ]))


(define (render-assignment-report/txt ar)
  (match ar
    [(struct assignment-report (name short-name lang prs))
     (string-append
      (format "ASSIGNMENT: ~a\n" name)
      (format " Language: ~a\n" lang)
      (format " Passed ~a out of ~a tests.\n" (count-passed-checks/assignment ar) 
              (count-total-checks/assignment ar))
      "\n"
      (apply string-append
             (map (λ(pr) (string-append (render-problem-report/txt pr) "\n"))
                  prs))
      )]
    ))



;;=================================================================================

(provide (struct-out problem)
         (struct-out assignment)
         (struct-out problem-report)
         (struct-out assignment-report)
         )

(provide/contract
 [check? contract?]
 [run-check (-> check? (or/c #f (any/c . -> . any)) (cons/c boolean? check?))]
 [check/problem (-> problem? (or/c #f (any/c . -> . any)) (or/c #f string?) problem-report?)]
; [check/assignment (-> assignment? (listof (or/c #f (cons/c string? bytes?))) assignment-report?)]
 [check/assignment
  (->d ([the-a assignment?] [subs (listof (or/c #f (cons/c string? bytes?)))])
       #:pre-cond (or (not subs)
                      (= (length (assignment-problems the-a)) (length subs)))
       [_ assignment-report?] )]
 [problem-check-timeout (parameter/c (or/c (>=/c 0) #f))]
 [render-check/txt (-> check? string?)]
 [render-check-result/txt (-> (cons/c boolean? check?) string?)]
 [render-problem-report/txt (-> problem-report? string?)]
 [render-assignment-report/txt (-> assignment-report? string?)]
 [count-total-checks/assignment (-> assignment-report? number?)]
 [count-failed-checks/assignment (-> assignment-report? number?)]
 [count-passed-checks/assignment (-> assignment-report? number?)]
 )
 


     