;;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :mlep)

(defun step-function (x)
  (if (> x 0) 1 0))

(defun d-step-function (x)
  (if (= x 0) 1 0))

(defun sigmoid (x)
  (/ 1.0d0 (+ 1.0d0 (expt +e+ (- x)))))

(defun d-sigmoid (x)
  (* x (- 1.0d0 x)))

(defun d-tanh (x)
  (- 1.0d0  (square x)))

(defparameter *diff-function-dict* (make-hash-table :size 2))
(setf (gethash #'step-function *diff-function-dict*) #'d-step-function)
(setf (gethash #'sigmoid *diff-function-dict*) #'d-sigmoid)
(setf (gethash #'tanh *diff-function-dict*) #'d-tanh)

(defun diff (func)
  (let ((result (gethash func *diff-function-dict*)))
    (if result result
      (error "No derivative function of ~a is known" func))))