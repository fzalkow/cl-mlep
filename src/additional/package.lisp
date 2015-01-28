;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :cl-user)

(defpackage :mlep-add
  (:use :cl :mlep :lla :cl-num-utils.matrix)
  (:export
   :principal-component-analysis :transform)
  
  (:import-from :mlep
   :list-to-2d-array :map-pointwise :multiply-matrices :run)
  (:import-from :lla
   :svd :svd-d :svd-u :svd-vt)
  (:import-from :cl-num-utils.matrix
   :diagonal-matrix-elements)
  (:documentation "@code{mlep-add} contains all parts of @code{mlep} that don't run without dependencies. Currently @a[https://github.com/tpapp/lla]{@code{lla}} and @a[https://github.com/tpapp/cl-num-utils]{@code{cl-num-utils}} are needed."))