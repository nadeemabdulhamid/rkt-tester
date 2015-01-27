#lang racket

#|

*** FROM THE DIRECTORY WHICH CONTAINS STUDENT FOLDERS ***
This should be run as:
         gracket    csc-auto-check.rkt   --batch assignment-specfile.rkt

You can also specify specific folders to check against the assignment spec:
         gracket    csc-auto-check.rkt   --batch assignment-specfile.rkt  folder1 folder2

Or you can have a GUI interface:
         gracket    csc-auto-check.rkt

|#


; - gracket csc120-check-dir [--gui] [--script] [--text] [<assignment-file.rkt> <directory>]
; an assignment file should export 'the-assignment : assignment?' (from checker2)

;(display (find-system-path 'run-file))
;(newline)

(require racket/gui)
(require "checker2.rkt")


;; (position-of 'd '(a b c d e f)) ==> 3
(define/contract
  (position-of v lst)
  (-> any/c list? (or/c natural-number/c #f))
  (let ([tl (member v lst)])
    (and tl (- (length lst) (length tl)))))

; (or (position-of ...) 0)

;; loads, evaluates, and import the 'the-assignment' definition from a file
(define/contract
  (load-assignment afile)
  (-> path-string? assignment?)
  (dynamic-require afile 'the-assignment))


;; produces a list of file names ending with .rkt extension in the given directory
(define/contract
  (rkt-files-in-dir dir)
  (-> path-string? (listof string?))
  (map path->string
       (filter
        (λ(pth)
          (and (file-exists? (build-path dir pth))
               (bytes=? #"rkt" (filename-extension pth))
               (not (char=? #\_ (string-ref (path->string (file-name-from-path pth)) 0)))
               (not (char=? #\. (string-ref (path->string (file-name-from-path pth)) 0)))))
        (directory-list dir))))


; check-assignment/dir : assignment? (listof path-string? or #f) -> assignment-report?
; given assignment and list of matching file names, load bytes from files, 
; check and produce an assignment report
(define/contract
  (check-assignment/dir the-a files)
  (-> assignment? (listof (or/c #f path-string?)) assignment-report?)
  (let ([submits   ; #f or (filename . bytes?)
         (map
          (λ(f)
            (if (false? f) #f
                (cons (some-system-path->string (file-name-from-path f))
                      (file->bytes f))
                  ))
          files)])
    (check/assignment the-a submits)))


(define/contract
  (find-submit-files-in-dir the-a submit-dir)
  (-> assignment? path-string? (listof (or/c #f string?)))
  (match the-a
    [(struct assignment (name short-name lang (list probs ...)))
     (map
      (λ(p)
        (let* ([name (problem-filename p)]
               [look-for (build-path submit-dir name)])
          (and (file-exists? look-for) name)
        ))
      probs)
     ]))


(define/contract
  (write-assignment-report/txt ar dir)
  (-> assignment-report? path-string? void)
  (let-values ([(base name must-be-dir?) (split-path dir)])
    (let ([report-name (build-path dir (string-append "_" ;(path->string name)
                                                      ;"-" (assignment-report-short-name ar) "-"
                                                      "auto-check-report.txt"))])
      (with-output-to-file report-name
        (λ() (display (render-assignment-report/txt ar)))
        #:exists 'replace
        ))))



;;==============================================================================
;;==============================================================================



;;==============================================================================
;;==============================================================================
;; GUI STUFF


(define/contract
  (file-match-gui func aspec adir submit-files)
  (-> (-> assignment? (listof (or/c #f path-string?)) any)
      assignment?
      path-string?
      (listof (or/c #f string?))
      any)
  (local [
          (define filematch-frame 
            (new frame% 
                 [label "Assignment/Submit File Match"]
                 ))
          
          (define choices-panel (new vertical-panel% [parent filematch-frame]))         
          
          (define button-panel (new horizontal-panel% [parent filematch-frame]))

          (define okb
            (new button%
                 [label "OK"]
                 [parent button-panel]
                 [callback (λ(btn ev) 
                             (let ([chcs (map (λ(c)
                                                (let ([s (send c get-string-selection)])
                                                  (if (string=? "----" s) #f (build-path adir s))))
                                              (send choices-panel get-children))])
                               ;(display chcs)(newline)
                               (func aspec chcs)
                               )
                             (send filematch-frame show #f)
                             )]
                 ))

          (define cancelb
            (new button%
                 [label "Quit"]
                 [parent button-panel]
                 [callback (λ(btn ev) (send filematch-frame show #f))]
                 ))
          ]
    (let ([rkt-files (rkt-files-in-dir adir)])
      ;(printf "~a ~a\n" adir rkt-files)
      (map
       (λ(pth prob)
         (new choice% [label (problem-name prob)]
              [parent choices-panel]
              ;[choices (list "aaaa" "bbbb")])
              [choices (cons "----" rkt-files)]
              [selection (add1 (or (position-of pth rkt-files) -1))])
       )
      submit-files (assignment-problems aspec)
      ))
    (send choices-panel set-alignment 'left 'top)
    (send filematch-frame set-alignment 'left 'top)
    (send filematch-frame show #t)
    ))



;; setup-gui : (path-string(aspec file)  path-string(adir) -> any) -> any
(define/contract
  (setup-gui func [init-aspec #f] [init-adir #f])
  (->* ((-> path-string? path-string? any) )
       ((or/c #f path-string?) (or/c #f path-string?))
       any)
  (local [
    (define ASPEC init-aspec)
    (define ADIR init-adir)
    
    (define setup-frame
      (new frame%
           [label "Select Assignment and Directory"]
           ))
    
    (define info1
      (new message%
           [label "Select assignment specification and directory"]
           [parent setup-frame]))
    (define info2
      (new message%
           [label "containing all files for submission."]
           [parent setup-frame]))
    
    (define af-panel (new horizontal-panel% [parent setup-frame]))
    
    (define af-label
      (new message% 
           [label (format "Current assignment spec: ~a" (or (and ASPEC (file-name-from-path ASPEC))
                                                            "(none)"))]
           [parent af-panel]
           [auto-resize #t]
           ))
    
    (define af-button
      (new button%
           [label "Select..."]
           [parent af-panel]
           [callback (λ(btn ev)
                       (match 
                           (get-file "Select assignment spec"
                                     setup-frame
                                     #f
                                     #f
                                     "rkt"
                                     '()
                                     '(("Any" "*.rkt")))
                         [#f (void)]
                         [pth (set! ASPEC pth)
                              (send af-label set-label
                                    (format "Current assignment spec: ~a"
                                            (file-name-from-path ASPEC)))
                              ]))]
           ))
    
    (define dir-panel (new horizontal-panel% [parent setup-frame]))
    
    (define dir-label
      (new message%
           [label (format "Directory of files for assignment: ~a" (or ADIR "(none"))]
           [parent dir-panel]
           [auto-resize #t]
           ))
    
    (define dir-button
      (new button%
           [label "Choose..."]
           [parent dir-panel]
           [callback (λ(btn ev)
                       (match
                           (get-directory "Choose assignment files directory"
                                          setup-frame)
                         [#f (void)]
                         [pth (set! ADIR pth)
                              (send dir-label set-label
                                    (format "Directory of files for assignment: ~a"
                                            (file-name-from-path ADIR)))
                              ]))]
           ))
    
    (define ctrl-panel (new horizontal-panel% [parent setup-frame]))
    
    (define ok-button
      (new button%
           [label "OK"]
           [parent ctrl-panel]
           [callback (λ(btn evt)
                       (send setup-frame show #f)
                       (if (or (not ASPEC) (not ADIR))
                           (message-box "Error" "You must specify both an assignment specification file\nand a submission directory." setup-frame)
                           (func ASPEC ADIR)
                           ;(printf "~a - ~a\n" ASPEC ADIR)
                           )
                       )]
           ))
    
    (define quit-button
      (new button%
           [label "Quit"]
           [parent ctrl-panel]
           [callback (λ(btn ev) (send setup-frame show #f))]
           ))
    ]
    (send setup-frame set-alignment 'left 'top)
    (send setup-frame show #t)))





;;==============================================================================
;;==============================================================================

#|
(define/contract 
  the-assignment (parameter/c (or/c #f assignment?)) 
  (make-parameter #f))
(define/contract 
  submit-dirs (parameter/c (listof path-string?))
  (make-parameter (list (current-directory))))

(command-line
   #:program "csc120-check.rkt"
;   #:argv (if (zero? (vector-length (current-command-line-arguments)))
;              '#("assignment-lab3.rkt" "xstuff")
;              (current-command-line-arguments))
   #:once-any
   ["--gui"    "Show window to select assignment and directory"
               (display "GUI\n")]
   ["--script" "Run without requesting user to match files"
               (display "SCRIPT\n")]
   #:args
   (assignment-file . submit-directories)
   (the-assignment (dynamic-require assignment-file 'the-assignment))
   (unless (empty? submit-directories)
     (submit-dirs submit-directories))
   #t
   )
|#


#|
(display
 (apply 
  string-append
  (map   
   (λ(dir)
     (render-assignment-report/txt (check-assignment/dir (the-assignment) 
                                                         (find-submit-files-in-dir
                                                          (the-assignment)
                                                          dir))))
   (submit-dirs))))
|#


;; find-immediate-subdirs : path -> listOf[path]
(define (find-immediate-subdirs [top-path #f])
  (reverse
   (fold-files (lambda (path kind acc) 
                 (if (and (directory-exists? path) (not (eq? path top-path)))
                     (values (cons path acc) #f) 
                     acc))
               null top-path #f)))


(define (gui-main)
  (setup-gui
      (λ(aspec-file dir)
        (let* ([the-assignment (load-assignment aspec-file)]
               [submit-files (find-submit-files-in-dir
                           the-assignment dir)])
       (file-match-gui
        (λ(the-a submits)
          (write-assignment-report/txt
           (check-assignment/dir the-a submits)
           dir))
        the-assignment dir submit-files)))))


(define (batch-main aspec-file submit-dirs)
  (for ([dir submit-dirs])
    (printf "processing ~a\n" dir)
    (let* ([the-assignment (load-assignment aspec-file)]
           [submit-files 
            (map (λ(n) (and n (build-path dir n)))
                 (find-submit-files-in-dir
                          the-assignment dir))])
      (write-assignment-report/txt
       (check-assignment/dir the-assignment submit-files)
       dir))
    ))

(define (main)
  (let ([aspec-file #f]
        [submit-dirs '()])
    (command-line
     #:program "csc-auto-check.rkt"
     #:once-any
     ["--batch" assignment-file "Run in batch mode (no GUI)"
                 (set! aspec-file assignment-file)]
     #:args
     submit-directories
     (set! submit-dirs
           (if (empty? submit-directories)
               (find-immediate-subdirs)
               submit-directories)))

    (if aspec-file
        (if (empty? submit-dirs)
            (error 'main "No submit directories specified on command line in batch mode.")
            (batch-main aspec-file submit-dirs))
        (gui-main)    
     )))

(main)






