;;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :mlep-add)

(defclass principal-component-analysis ()
  ((data-set
    :initarg :data-set
    :initform nil
    :accessor data-set
    :documentation "The data-set to be analyzed.")
   (data
    :initarg :data
    :initform nil
    :accessor data
    :documentation "Converted data.")
   (means
    :initarg :means
    :initform nil
    :accessor means
    :documentation "Means of data.")
   (unitary-matrix1
    :initarg :unitary-matrix1
    :initform nil
    :accessor unitary-matrix1
    :documentation "Unitary matrix U.")
   (unitary-matrix2
    :initarg :unitary-matrix2
    :initform nil
    :accessor unitary-matrix2
    :documentation "Unitary matrix V.")
   (singular-values
    :initarg :singular-values
    :initform nil
    :accessor singular-values
    :documentation "The singular values for every matrix."))
  (:documentation "Principal Component Analysis by Singular Value Decomposition"))     
      
(defmethod run ((instance principal-component-analysis) &key)
  (with-slots (data-set data means unitary-matrix1 unitary-matrix2 singular-values) instance
    (setf data (list-to-2d-array data-set :element-type 'double-float))
    (let ((items (first (array-dimensions data)))
          (dims (second (array-dimensions data))))
      (setf means (make-array dims :initial-element 0d0 :element-type 'double-float))
      ; subtract off the mean for each dimension
      (dotimes (d dims)
        (dotimes (i items)
          (setf (aref means d) (+ (aref means d) (aref data i d)))))
      (setf means (map-pointwise #'(lambda (x) (/ x items)) means))
      (dotimes (d dims)
        (dotimes (i items)
          (setf (aref data i d) (- (aref data i d) (aref means d)))))
      ; run svd
      (let ((my-svd (svd data)))
        (setf unitary-matrix1 (svd-u my-svd))
        (setf unitary-matrix2 (svd-vt my-svd))
        (setf singular-values (diagonal-matrix-elements (svd-d my-svd)))
        (list unitary-matrix1 unitary-matrix2 singular-values)))))

(defmethod transform ((instance principal-component-analysis) &key components inverse new-data)
  (with-slots (data means unitary-matrix2) instance
    (when (and new-data (consp new-data))
      (setf new-data (list-to-2d-array new-data)))
    (let ((items (first (array-dimensions (if new-data new-data data))))
          (dim (second (array-dimensions (if new-data new-data data)))))
      (unless (or data new-data)
        (run instance))
      (unless components
        ; set components with all dimensions
        (setf components dim))
      (let ((reduced-unitary-matrix2 (make-array (list components dim) :displaced-to unitary-matrix2 :element-type 'double-float)))
        (if inverse
            (let ((result (multiply-matrices (if new-data new-data data) reduced-unitary-matrix2)))
              ; add mean on result
              (dotimes (d components result)
                (dotimes (i items)
                  (setf (aref result i d) (+ (aref means d) (aref result i d))))))
          (multiply-matrices (if new-data new-data data) (transpose reduced-unitary-matrix2)))))))