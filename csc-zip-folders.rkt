#lang scheme

#|
*** FROM THE DIRECTORY WHICH CONTAINS STUDENT FOLDERS ***

This should be run as:
    gracket ../zip-folders.rkt

*** FROM THE DIRECTORY WHICH CONTAINS STUDENT FOLDERS ***
|#

(require file/zip)

;; find-immediate-subdirs : path -> listOf[path]
(define (find-immediate-subdirs [top-path #f])
  (reverse
   (fold-files (lambda (path kind acc) 
                 (if (and (directory-exists? path) (not (eq? path top-path)))
                     (values (cons path acc) #f) 
                     acc))
               null top-path #f)))

(define (main)
  (zip-verbose true)
  (for-each
   (lambda (pth)
     (zip (string-append 
           (path->string (file-name-from-path pth))
           "assignsubmission_file_graded"
           ".zip")
          pth))
   (find-immediate-subdirs))
  )

(main)