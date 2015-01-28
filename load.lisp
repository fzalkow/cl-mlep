;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(unless (find-package :asdf)
  (require :asdf))

(let ((directory (make-pathname :directory (pathname-directory *load-truename*))))
  (unless (member directory asdf:*central-registry*)
    (push directory asdf:*central-registry*)))

(asdf:operate 'asdf:load-op :mlep)