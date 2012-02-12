#lang racket

(require rackunit
         "eval2.rkt"
         "checker2.rkt")

(define 
  e1 (make-module-evaluator/submission
      #"(module h (lib \"htdp-intermediate.ss\" \"lang\")
(define CONST 4)

(define (compute-bmi a b)
  (if (< a b) a \"bigger\"))
        )" 
      '(htdp intermediate)))


;;######################################################################
(define (string/symbol? x)
  (or (string? x) (symbol? x)))

(define LAB3-CHECK
  (assignment "Lab 3"
              "lab3"
              '(htdp intermediate)
              (list
               (problem "1. Body Mass Index"
                        "lab3-bmi.rkt"
                        `(
                          (proc compute-bmi 2)
                          (proc interpret-bmi 1)
                          (type (compute-bmi 60 150) "a number" ,number?)
                          (type (interpret-bmi 20) "a string or symbol" ,string/symbol?)
                          (test (compute-bmi 60 150) 29.3)
                          (test (interpret-bmi 29) "overweight")
                          (test (interpret-bmi 20) "normal")
                          (test (interpret-bmi 15) "underweight")
                          (test (interpret-bmi 30) "obese")
                          ))
               (problem "2. Movie Fees"
                        "lab3-movie-fees.rkt"
                        `(
                          (proc late-fee 1)
                          (type (late-fee 5) "a number" ,number?)
                          ))
               )))


(define LAB3-RESULT
  (assignment-report "Lab 3"
                     "lab3"
                     '(htdp intermediate)
                     (list
                      (problem-report "1. Body Mass Index"
                                      "lab3-bmi.rkt"
                                      #t  ; eval'd?
                                      #f  ; timeout?
                                      #f  ; fname good?
                                      #f  ; covered?
                                      `(
                                        (#t proc compute-bmi 2)
                                        (#f proc interpret-bmi 1)
                                        (#t type (compute-bmi 60 150) "a number" ,number?)
                                        (#f type (interpret-bmi 20) "a string or symbol"
                                            ,string/symbol?)
                                        (#f test (compute-bmi 60 150) 29.3)
                                        (#f test (interpret-bmi 29) "overweight")
                                        (#f test (interpret-bmi 20) "normal")
                                        (#f test (interpret-bmi 15) "underweight")
                                        (#f test (interpret-bmi 30) "obese")
                                        ))
                      (problem-report "2. Movie Fees"
                                      "lab3-movie-fees.rkt"
                                      #f
                                      #f  ; timeout?
                                      #f
                                      #f
                                      `(
                                        (#f proc late-fee 1)
                                        (#f type (late-fee 5) "a number" ,number?)
                                        ))
                      )))


(define LAB3-RESULT/TIMEOUT
  (assignment-report "Lab 3"
                     "lab3"
                     '(htdp intermediate)
                     (list
                      (problem-report "1. Body Mass Index"
                                      "lab3-bmi.rkt"
                                      #t  ; eval'd?
                                      #t  ; timeout?
                                      #t  ; fname good?
                                      #f  ; covered?
                                      `(
                                        (#f proc compute-bmi 2)
                                        (#f proc interpret-bmi 1)
                                        (#f type (compute-bmi 60 150) "a number" ,number?)
                                        (#f type (interpret-bmi 20) "a string or symbol"
                                            ,string/symbol?)
                                        (#f test (compute-bmi 60 150) 29.3)
                                        (#f test (interpret-bmi 29) "overweight")
                                        (#f test (interpret-bmi 20) "normal")
                                        (#f test (interpret-bmi 15) "underweight")
                                        (#f test (interpret-bmi 30) "obese")
                                        ))
                      (problem-report "2. Movie Fees"
                                      "lab3-movie-fees.rkt"
                                      #f
                                      #f  ; timeout?
                                      #f
                                      #f
                                      `(
                                        (#f proc late-fee 1)
                                        (#f type (late-fee 5) "a number" ,number?)
                                        ))
                      )))

(define LAB3-RESULT/NOEVAL
  (assignment-report "Lab 3"
                     "lab3"
                     '(htdp intermediate)
                     (list
                      (problem-report "1. Body Mass Index"
                                      "lab3-bmi.rkt"
                                      #f  ; eval'd?
                                      #f  ; timeout?
                                      #t  ; fname good?
                                      #f  ; covered?
                                      `(
                                        (#f proc compute-bmi 2)
                                        (#f proc interpret-bmi 1)
                                        (#f type (compute-bmi 60 150) "a number" ,number?)
                                        (#f type (interpret-bmi 20) "a string or symbol"
                                            ,string/symbol?)
                                        (#f test (compute-bmi 60 150) 29.3)
                                        (#f test (interpret-bmi 29) "overweight")
                                        (#f test (interpret-bmi 20) "normal")
                                        (#f test (interpret-bmi 15) "underweight")
                                        (#f test (interpret-bmi 30) "obese")
                                        ))
                      (problem-report "2. Movie Fees"
                                      "lab3-movie-fees.rkt"
                                      #f
                                      #f  ; timeout?
                                      #f
                                      #f
                                      `(
                                        (#f proc late-fee 1)
                                        (#f type (late-fee 5) "a number" ,number?)
                                        ))
                      )))
;;=================================================================================
;;======== run-check TESTS

(define (check-run-check/fail chks e)
  (for ([c chks])
    (check-equal? (take (run-check c e) 3) (take (cons #f c) 3))))
   ; take 3 because beyond that, procedure references in the chks seem to get mangled (?)

(define (check-run-check/pass chks e)
  (for ([c chks])
    (check-equal? (take (run-check c e) 3) (take (cons #t c) 3))))

(check-run-check/pass `( 
                        (defined compute-bmi)
                        (proc compute-bmi 2)
                        (type (compute-bmi 1 2) "a number" ,number?)
                        (type CONST "a number" ,number?)
                        (type (number->string CONST) "a string" ,string?)
                        (type compute-bmi "a function" ,procedure?)
                        (test (compute-bmi 1 2))
                        (test (> CONST 0))
                        (test (compute-bmi 1 2) 1)
                        (test (compute-bmi 2 1) "bigger")
                        (test CONST 4)
                        (test (compute-bmi 1 2) 1.1 ,(λ(a b) (<= (- b .5) a (+ b .5))))
                        )
                      e1)

(check-run-check/fail `(
                        (defined interpret-bmi)      ; not defined
                        
                        (proc CONST 1)               ; defined, not proc
                        (proc compute-bmi 1)         ; wrong arity
                        (proc compute-bmi 3)
                        (proc interpret-bmi 1)       ; not defined
                        
                        (type (compute-bmi 2 1) "a number" ,number?)
                        (type (interpret-bmi 1) "a string" ,string?)
                        
                        (test (interpret-bmi 1))
                        (test (= CONST -1))
                        
                        (test (interpret-bmi 1) "thin")
                        (test (compute-bmi 2 1) 1)
                        
                        (test (compute-bmi 1 2) 12.1 ,(λ(a b) (<= (- b .5) a (+ b .5))))
                        )
                      e1)

(check-run-check/fail `(
                        (defined compute-bmi)
                        (proc CONST 1)          
                        (proc compute-bmi 1)    
                        (proc compute-bmi 3)
                        (proc interpret-bmi 1) 
                        (type (compute-bmi 2 1) "a number" ,number?)
                        (type (interpret-bmi 1) "a string" ,string?)
                        (type CONST "a number" ,number?)
                        (test (interpret-bmi 1))
                        (test (= CONST -1))
                        (test (compute-bmi 1 2))
                        (test (> CONST 0))
                        (test (interpret-bmi 1) "thin")
                        (test (compute-bmi 2 1) 1)
                        (test (compute-bmi 1 2) 1)
                        (test (compute-bmi 1 2) "bigger")
                        (test CONST 4)
                        (test (compute-bmi 1 2) 1.1 ,(λ(a b) (<= (- b .5) a (+ b .5))))
                        (test (compute-bmi 1 2) 12.1 ,(λ(a b) (<= (- b .5) a (+ b .5))))
                        )
                      #f)
 

(check-equal?
 (check/problem
  (problem "1. Body Mass Index"
           "lab3-bmi.rkt"
           `(
             (proc compute-bmi 2)
             (proc interpret-bmi 1)
             (type (compute-bmi 60 150) "a number" ,number?)
             (type (compute-bmi 160 150) "a number" ,number?)
             (type (interpret-bmi 20) "a string or symbol" ,string/symbol?)
             (test (compute-bmi 60 150) 29.3)
             (test~ (compute-bmi 60 150) 60.1 .5)
             (test~ (compute-bmi 60 150) 70.1 .5)
             (test (interpret-bmi 29) "overweight")
             (test (interpret-bmi 20) "normal")
             (test (interpret-bmi 15) "underweight")
             (test (interpret-bmi 30) "obese")
             ))
  e1
  "body-mass-index.rkt")
 (problem-report "1. Body Mass Index"
                 "lab3-bmi.rkt"
                 #t  ; checked
                 #f  ; timeout?
                 #f  ; filename good
                 #f  ; coverage good
                 `(
                   (#t proc compute-bmi 2)
                   (#f proc interpret-bmi 1)
                   (#t type (compute-bmi 60 150) "a number" ,number?)
                   (#f type (compute-bmi 160 150) "a number" ,number?)
                   (#f type (interpret-bmi 20) "a string or symbol" ,string/symbol?)
                   (#f test (compute-bmi 60 150) 29.3)
                   (#t test~ (compute-bmi 60 150) 60.1 .5)
                   (#f test~ (compute-bmi 60 150) 70.1 .5)
                   (#f test (interpret-bmi 29) "overweight")
                   (#f test (interpret-bmi 20) "normal")
                   (#f test (interpret-bmi 15) "underweight")
                   (#f test (interpret-bmi 30) "obese")
                   )))
  
(check-equal? 
 (check/assignment LAB3-CHECK '(("hi" . #"(module h (lib \"htdp-intermediate.ss\" \"lang\")
(define CONST 4)

(define (compute-bmi a b)
  (if (< a b) a \"bigger\"))
        )") 
                                #f))
 LAB3-RESULT)

(check-equal? 
 (parameterize ([problem-check-timeout 2])
   (check/assignment LAB3-CHECK '(("lab3-bmi.rkt" . #"(module h (lib \"htdp-intermediate.ss\" \"lang\")
(define CONST 4)

(define (compute-bmi a b)
  (if (< a b) (compute-bmi a b) \"bigger\"))
        )")
                                #f)))
 LAB3-RESULT/TIMEOUT)

(check-equal? 
 (parameterize ([problem-check-timeout 2])
   (check/assignment LAB3-CHECK '(("lab3-bmi.rkt" . #"(module h (lib \"htdp-intermediate.ss\" \"lang\")
(define CONST 4)

(define (compute-bmi a b)
  (if (< a b) (compute-bmi a b) \"bigger\"))
        )

(compute-bmi 1 2)
")
                                #f)))
 LAB3-RESULT/NOEVAL)




;;=================================================================================
;; TEST RENDERING

(check-equal? (render-check/txt '(proc compute-bmi 2))
              "Is 'compute-bmi' defined as a function of 2 parameters?")
(check-equal? (render-check/txt '(proc compute-bmi 1))
              "Is 'compute-bmi' defined as a function of 1 parameter?")
(check-equal? (render-check/txt '(defined CHECK))
              "Is 'CHECK' defined?")
(check-equal? (render-check/txt `(type (compute-bmi 60 150) "a number" ,number?))
              "Does (compute-bmi 60 150) produce a number?")
(check-equal? (render-check/txt `(type (interpret-bmi 20) "a string or symbol" ,string/symbol?))
              "Does (interpret-bmi 20) produce a string or symbol?")
(check-equal? (render-check/txt '(test (compute-bmi 60 150)))
              "Does (compute-bmi 60 150) produce a result other than 'false'?")
(check-equal? (render-check/txt '(test (compute-bmi 60 150) 21.5))
              "Does (compute-bmi 60 150) produce an expected result?")
(check-equal? (render-check/txt '(test~ (compute-bmi 60 150) 21.5 .1))
              "Does (compute-bmi 60 150) produce 21.5 +/- 0.1?")
(check-equal? (render-check/txt `(test (compute-bmi 60 150) 21.5 ,eq?))
              "Does (compute-bmi 60 150) produce an expected result?")

(check-equal? (render-check-result/txt '(#t proc compute-bmi 2))
              "   PASS: Is 'compute-bmi' defined as a function of 2 parameters?\n")
(check-equal? (render-check-result/txt '(#f proc compute-bmi 1))
              " x FAIL: Is 'compute-bmi' defined as a function of 1 parameter?\n")
(check-equal? (render-check-result/txt '(#t defined CHECK))
              "   PASS: Is 'CHECK' defined?\n")
(check-equal? (render-check-result/txt `(#f type (compute-bmi 60 150) "a number" ,number?))
              " x FAIL: Does (compute-bmi 60 150) produce a number?\n")

(check-equal? (count-total-checks/assignment LAB3-RESULT) 19)
(check-equal? (count-failed-checks/assignment LAB3-RESULT) 15)
(check-equal? (count-passed-checks/assignment LAB3-RESULT) 4)

(check-equal? 
 (render-problem-report/txt
  (problem-report "1. Body Mass Index"
                                      "lab3-bmi.rkt"
                                      #t  ; eval'd?
                                      #f  ; timeout?   <---- #f is actually good
                                      #t  ; fname good?
                                      #f  ; covered?
                                      `(
                                        (#f proc compute-bmi 2)
                                        (#t proc interpret-bmi 1)
                                        )))
 #<<STOP
1. Body Mass Index
 Passed 4 out of 6 tests.
   PASS: File name matches 'lab3-bmi.rkt'?
   PASS: File evaluated without error?
   PASS: File ran without timeout?
 x FAIL: Tests cover all expressions?
   ----
 x FAIL: Is 'compute-bmi' defined as a function of 2 parameters?
   PASS: Is 'interpret-bmi' defined as a function of 1 parameter?

STOP
)

#;(check-equal?
 (render-assignment-report/txt LAB3-RESULT)
 #<<STOP
ASSIGNMENT: Lab 3
 Language: (htdp intermediate)
 Passed 5 out of 19 tests.

PROBLEM: 1. Body Mass Index
 Passed 4 out of 13 tests.
   PASS: File name matches 'lab3-bmi.rkt'?
   PASS: File evaluated without error?
   PASS: File ran without timeout?
 x FAIL: Tests cover all expressions?
   ----
 x FAIL: Is 'compute-bmi' defined as a function of 2 parameters?
   PASS: Is 'interpret-bmi' defined as a function of 1 parameter?

PROBLEM: 2. Movie Fees
 Passed 1 out of 6 tests.
 x FAIL: File name matches 'lab3-movie-fees.rkt'?
 x FAIL: File evaluated without error?
   PASS: File ran without timeout?
 x FAIL: Tests cover all expressions?
   ----
 x FAIL: Is 'late-fee' defined as a function of 1 parameter?
 x FAIL: Does (late-fee 5) produce a number?

STOP
  )