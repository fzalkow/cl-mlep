;;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :mlep)

(defclass max-likelihood ()
  ((data-set
    :initarg :data-set
    :initform nil
    :accessor data-set
    :documentation "The data-set to be analyzed.")
   (degrees-of-freedom
    :initarg :ddof
    :initform 0
    :accessor degrees-of-freedom
    :documentation "Delta Degrees of Freedom. Divisor is length of @code{data-set} minus @code{degrees-of-freedom}."))
  (:documentation "With max-likelihood one can estimate the parameters of the normal distributed probability density function that fits the data-set."))
   

(defmethod run ((instance max-likelihood) &key)
  (with-slots (data-set degrees-of-freedom) instance
    (let ((len (length data-set)))
      (when (numberp (first data-set))
        (setf data-set (mapcar #'list data-set)))
      (let* ((mean (map 'vector #'(lambda (dimension) (mean dimension)) (transpose data-set)))
             (co-variance (map-pointwise #'(lambda (x) (/ x (- len degrees-of-freedom)))
                                         (reduce #'(lambda (a b) (map-pointwise #'+ a b))
                                                (mapcar #'(lambda (item)
                                                            (let ((tmp (mapcar #'- item (coerce mean 'list))))
                                                              (outer-product tmp tmp)))
                                                        data-set)))))
        (list mean co-variance)))))