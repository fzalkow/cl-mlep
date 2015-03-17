;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage :mlep-add-asd
  (:use :cl :asdf))
  
(defsystem :mlep-add
    :name "mlep-add"
    :version "0.0.1"
    :maintainer "Frank Zalkow <frank_zalkow@web.de>"
    :author "Frank Zalkow <frank_zalkow@web.de>"
    :licence "The MIT License <http://opensource.org/licenses/MIT>"
    :depends-on (:mlep :cffi :lla :cl-num-utils)
    :serial t
    :pathname #P"src/additional/"
    :components ((:file "package")
                 (:file "pca")))

; even though the CFFI manual states that you not have to use
; CFFI:*FOREIGN-LIBRARY-DIRECTORIES* I need it to contain the
; directory for my dynlibs
(defmethod perform :after ((op load-op) c)
  (if (string-equal (slot-value c 'asdf::name) "cffi")
      (set (intern "*FOREIGN-LIBRARY-DIRECTORIES*" :cffi)
           (cons #P"/usr/lib/"
                 (eval (intern "*FOREIGN-LIBRARY-DIRECTORIES*"
                               :cffi))))))