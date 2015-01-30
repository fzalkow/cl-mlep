;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage :mlep-add-asd
  (:use :cl :asdf))

; even though the CFFI manual states that you not have to use
; CFFI:*FOREIGN-LIBRARY-DIRECTORIES* I need it to contain the
; directory for my dynlibs
(defsystem :mlep-add/push-cffi-dir
  :name "mlep-add/push-cffi-dir"
  :pathname #P"src/additional/"
  :depends-on (:cffi)
  :serial t
  :components ((:file "push-cffi-dir")))
  
(defsystem :mlep-add
    :name "mlep-add"
    :version "0.0.1"
    :maintainer "Frank Zalkow <frank_zalkow@web.de>"
    :author "Frank Zalkow <frank_zalkow@web.de>"
    :licence "The MIT License <http://opensource.org/licenses/MIT>"
    :defsystem-depends-on (:mlep-add/push-cffi-dir)
    :depends-on (:mlep :lla :cl-num-utils)
    :serial t
    :pathname #P"src/additional/"
    :components ((:file "package")
                 (:file "pca")))