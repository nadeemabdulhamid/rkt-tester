#lang racket

#|
*** FROM THE DIRECTORY WHICH CONTAINS STUDENT FOLDERS ***

This should be run as:
    gracket ../csc-collect-grades.ss 

*** FROM THE DIRECTORY WHICH CONTAINS STUDENT FOLDERS ***
|#

(require wxme)
(require racket/gui)
(require test-engine/racket-tests)

(define FILE-EXTENSION "(java|ss|rkt)")

;; meta-comments : String -> listof[String]
(define (meta-comments file-name)
  (let ([ED (new text%)])
    (send ED load-file file-name)
    (send ED get-flattened-text)
    (regexp-match* (regexp (string-append GRADE-COMMENT "[^\n]*")) (send ED get-flattened-text))))

; (meta-comments "/Users/nhamid/Documents/Data/berry/09fall/csc120/work/228-hw01/drasean.ford/hw1-1.ss")


(define GRADE-COMMENT "(?://##|;;##)")

;; extract-points : String Number -> Number
(define (extract-points a-line sum)
  (let ([match (regexp-match (regexp (string-append GRADE-COMMENT " *\\(([-+]?[0-9]*\\.?[0-9]*)\\)")) a-line)])
    (if match 
        (+ sum (string->number (second match)))
        sum)))

(check-expect (extract-points "//## (10)" 1) 11)
(check-within (extract-points "//## (10.5)" 1) 11.5 .1)
(check-within (extract-points "//## (.5)" 1) 1.5 .1)
(check-within (extract-points "//## (0.5)" 1) 1.5 .1)
(check-within (extract-points "//## (-.5)" 1) 0.5 .1)
(check-within (extract-points "//## (5.0)" 1) 6 .1)
(check-expect (extract-points "//## (-1) show the step where (double 3) ==> (* 3 2)" -3) -4)
(check-within (foldl extract-points 0 
                     '("//## (-1) hi" "//## ethi" "//## (-4) yo" "//## (6.2) hic"))
              1.2 .01)
                     
;; sum-points : String -> Number
(define (sum-points file-name)
  (foldl extract-points 0
         (meta-comments file-name)))

; (sum-points "/Users/nhamid/Documents/Data/berry/09fall/csc120/work/228-hw01/drasean.ford/hw1-1.ss")

;(test)

;; load-ss-file-as-text : Path -> listof[String]
(define (load-ss-file-as-text file-name)
  (let ([ED (new text%)])
    (send ED load-file file-name)
    (send ED get-flattened-text))
  )

;; process-line : String (String Number Number) -> (String Number Number)
;; (String Number Number) is (accumulated-output current-sum current-line)
(define (process-line a-line triple)
  ;(write (third triple))
  ;(write "\n")
  (let ([match-num (regexp-match (regexp (string-append GRADE-COMMENT " *\\(([-+]?[0-9]*\\.?[0-9]*)\\)(.*)"))
                                 a-line)]
        [match-com (regexp-match (regexp (string-append GRADE-COMMENT " *(.*)")) a-line)])
    (cond [match-num
           ;(write-string (first match-num)) (newline)
           (list
            (string-append (first triple)
                            "      > line " 
                           (number->string (third triple))
                           ": (" (second match-num) ") "
                           (third match-num)
                           "\n")                       
            (+ (string->number (second match-num)) (second triple))
            (add1 (third triple)))]
          [match-com
           (list
            (string-append (first triple)
                           "      > line " 
                           (number->string (third triple))
                           ": " (second match-com) "\n")
            (second triple)
            (add1 (third triple)))]
          [else (list (first triple) (second triple) (add1 (third triple)))])))


;; process-student-file : Path -> (String . Number)
;; produces a string listing of grader comments, and the total points
(define (process-student-file pth)
  (write-string (string-append "    " (path->string (file-name-from-path pth)) "\n"))
  (let ([res (foldl process-line (list "" 0 1) (cdddr (regexp-split "\n" 
                                                                    (load-ss-file-as-text pth))))])
    (cons (string-append (first res) "\n") (second res))))
;; cdddr --> discard the first three lines of metadata in the file


;; process-student-dir : Path -> String
(define (process-student-dir pth)
  (write-string (string-append "processing " (path->string (file-name-from-path pth)) "\n"))
  (let ([result
         (fold-files 
          ;; path type (String . Number) -> (String . Number)
          (lambda (f-pth f-type rep-pair)
            (if (and (symbol=? f-type 'file)
                     (regexp-match (regexp (string-append "\\." FILE-EXTENSION "$"))
                                   (path->string f-pth)))
                (let ([file-pair (process-student-file f-pth)])
                  (cons
                   (string-append (car rep-pair)
                                  "   " (path->string (file-name-from-path f-pth)) "\n"
                                  (car file-pair))
                 (+ (cdr file-pair) (cdr rep-pair))))
                rep-pair
                ;(if (eq? f-pth pth) rep-pair (values rep-pair #f))
                ))
          (cons (string-append (path->string (file-name-from-path pth)) "\n") 0)
          pth)])
    (string-append
     "===============================================\n"
     (car result)
     "   Total: " (number->string (cdr result))
     "\n===============================================\n\n")
    ))


#|
(write-string
(process-student-dir
 (second
  (find-files
   (lambda (pth) (directory-exists? pth))
   "/Users/nhamid/Documents/Data/berry/09fall/csc120/work/228-hw01")))
)
|#


;; find-immediate-subdirs : path -> listOf[path]
(define (find-immediate-subdirs [top-path #f])
  (reverse
   (fold-files (lambda (path kind acc) 
                 (if (and (directory-exists? path) (not (eq? path top-path)))
                     (values (cons path acc) #f) 
                     acc))
               null top-path #f)))

(define (main)
  (let ([all-rep
         (foldl (lambda (pth acc)
                  (let ([stu-rep (process-student-dir pth)])
                    (call-with-output-file 
                        (build-path pth 
                                    (string-append (path->string (file-name-from-path pth))
                                                   "-grade-report" 
                                                   ".txt"))
                      (lambda (op) (write-string stu-rep op))
                      #:exists 'replace)
                    (string-append acc stu-rep))
                  )
                ""
                (find-immediate-subdirs))
;              "/Users/nhamid/Documents/Data/berry/09fall/csc120/work/228-hw01"))
         ])
    (call-with-output-file
        (build-path (current-directory) "grade-report.txt")
      (lambda (op) (write-string all-rep op)) #:exists 'replace)
    
    (write-string "\nDone\n")
    (void)
    ))

(main)

;(test)


#|

228-hw01 Report

barbara.tomai
   hw1-1.ss
      > line x: (-1) blblakjfl
      > line y: (-2) .....
   hw1-2.ss


|#
