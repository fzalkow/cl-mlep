;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage :mlep-add-asd
  (:use :cl :asdf))

(in-package :mlep-add-asd)

(defsystem :mlep-add
    :name "mlep-add"
    :version "0.0.0"
    :maintainer "Frank Zalkow <frank_zalkow@web.de>"
    :author "Frank Zalkow <frank_zalkow@web.de>"
    :licence "CC BY 3.0 license <http://creativecommons.org/licenses/by/3.0>"
    :depends-on (:mlep :lla :cl-num-utils)
    :serial t
    :components ((:file "package")
                 (:file "pca")))