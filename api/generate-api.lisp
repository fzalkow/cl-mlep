;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(unless (find-package :atdoc)
  (if (find-package :quicklisp)
      (ql:quickload :atdoc)
    (error "ATDOC has to be loaded for generating the API documentation. QUICKLISP isn't available either.")))



(let ((mlep-load-file (make-pathname :directory (butlast (pathname-directory *load-truename*) 2)
                                     :name "load-with-add"
                                     :type "lisp")))
  (if (probe-file mlep-load-file)
      (load mlep-load-file)
	  (error "Cannot find load-file for CL-MLEP.")))
        
(atdoc:generate-html-documentation
 '(:mlep :mlep-add)
 (make-pathname :directory (pathname-directory *load-truename*))
 :index-title "cl-mlep API reference"
 :heading "cl-mlep API reference"
 :single-page-p t
 :include-internal-symbols-p nil
 :include-slot-definitions-p t
 :css (make-pathname :directory (pathname-directory *load-truename*) :name "api" :type "css"))


