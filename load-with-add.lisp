;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(ql:quickload :cffi)
(push #P"/usr/lib/" cffi:*foreign-library-directories*)
(ql:quickload :lla)

(let ((directory (pathname-directory *load-truename*)))
  (unless (find-package :lla)
    (error "LLA must be loaded first: https://github.com/tpapp/lla/"))
  (unless (find-package :asdf)
    (require :asdf))
  (unless (find-package :mlep)
    (load (make-pathname :directory directory :name "load" :type "lisp")))
  (let ((directory-add (make-pathname :directory (append directory '("src" "additional")))))
    (unless (member directory-add asdf:*central-registry*)
      (push directory-add asdf:*central-registry*)))
  (asdf:operate 'asdf:load-op :mlep-add))