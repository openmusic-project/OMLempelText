
;; ==================================================================================== 
;;                               OMLEMPELTEXT
;; ==================================================================================== 

;;; =================================================================================== 
;;;
;;;                       © 2001  Gerard Assayag  - IRCAM
;;;                       (revision et augmentation Karim Haddad)
;;;
;;; =================================================================================== 




(in-package :om)

;--------------------------------------------------
;Variable definiton with files to load 
;--------------------------------------------------

(defvar *lempeltext-lib-files* nil)
(setf *lempeltext-lib-files* (list 
                              (make-pathname :directory (append (pathname-directory *load-pathname*) (list "sources")) 
                                             :name "lempeltext" :type "lisp")
                              ))

;--------------------------------------------------
;Loading files 
;--------------------------------------------------

(mapc #'compile&load *lempeltext-lib-files*)

;--------------------------------------------------
; OM subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------
(defvar *subpackages-list* nil)
(setf *subpackages-list*
      '( ("lempeltext" nil nil (LZtextify LZtextGenerate) nil)
         ("lempeltext utils" nil nil (string->ascii ascii->string) nil)
         ))

;--------------------------------------------------
;filling packages
;--------------------------------------------------
(om::fill-library *subpackages-list*)

(om::set-lib-release 1.1)

(print "
;;;=============================================
;;; OMLempelText 1.1
;;; LZ engine for textfiles in OM
;;; Augemnted and revised by K. Haddad
;;;=============================================
")


