;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(unless (find-package :quicklisp)
  (error "You must have Quicklisp!"))

(ql:quickload :atdoc)
(ql:quickload :mlep-add)

(atdoc:generate-html-documentation
 '(:mlep :mlep-add)
 (make-pathname :directory (pathname-directory *load-truename*))
 :index-title "cl-mlep API reference"
 :heading "cl-mlep API reference"
 :single-page-p t
 :include-internal-symbols-p nil
 :include-slot-definitions-p t
 :css (make-pathname :directory (pathname-directory *load-truename*) :name "api" :type "css"))