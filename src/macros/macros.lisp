;;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :mlep)

; extension of dolist for more than one list
(defmacro do-lists (((&rest vars) (&rest lists) &optional result) &body body)
  (when (/= (list-length vars) (list-length lists))
    (error "`vars' and `lists' must be of same length!"))
  (let ((var-code)
        (list-vars (loop repeat (list-length vars) collect (gensym "do-lists"))))
    (do* ((list-vars list-vars (rest list-vars))
          (lists lists (rest lists))
          (vars vars (rest vars))
          (current-list (first lists) (first lists))
          (current-list-var (first list-vars) (first list-vars))
          (current-var (first vars) (first vars)))
         ((endp list-vars))
      (push `(,current-list-var ,current-list (rest ,current-list-var)) var-code)
      (push `(,current-var (first ,current-list-var) (first ,current-list-var)) var-code))
    `(do* ,(nreverse var-code)
          ((endp ,(first list-vars)) ,result)
       ,@body)))

; extension of dolist for holding the index
(defmacro dolist-with-index ((index list-item list &optional result) &body body)
  `(let ((,index 0))
     (dolist (,list-item ,list ,result)
       ,@body
       (incf ,index))))


(defmacro dotimes-fromto ((var from to &optional result) &body body)
  (let ((to-name (gensym)))
    `(do ((,var ,from (1+ ,var))
          (,to-name ,to))
         ((>= ,var ,to-name) ,result)
       ,@body)))