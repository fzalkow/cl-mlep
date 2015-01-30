(defpackage :mlep-add/push-cffi-dir
  (:use :cl :cffi))
  
(cl:in-package :mlep-add/push-cffi-dir)
  
(cl:push #P"/usr/lib/" cffi:*foreign-library-directories*)