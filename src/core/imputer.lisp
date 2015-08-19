;;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :mlep)

(defclass imputer ()
  ((data-set
    :initarg :data-set
    :initform nil
    :accessor data-set
    :documentation "The data-set to be analyzed.")
   (missing-value
    :initarg :missing-value
    :initform nil
    :accessor missing-value
    :documentation "The value that is recognized as a missing value.")
   (missing-value-test
    :initarg :missing-value-test
    :initform #'eq
    :accessor missing-value-test
    :documentation "The test-function for comparing each item in a data-set with this missing value.")
   (replacers
    :initform nil
    :accessor replacers
    :documentation "The values that are used for replacing missing values per column."))
  (:documentation "Replace missing value by the mean (for numercial data) or the mode (for categorical data)."))

(defmethod run ((instance imputer) &key)
  (with-slots (data-set missing-value missing-value-test replacers) instance
    (let ((without-missing
           (mapcar #'(lambda (column)
                       (remove-if #'(lambda (item)
                                      (funcall missing-value-test
                                               item missing-value))
                                  column))
                   (transpose data-set))))
      (setf replacers
            (map 'vector #'(lambda (column)
                             (if (every #'numberp column)
                                 (mean column)
                               (most-frequent-value column)))
                 without-missing)))))

(defmethod transform ((instance imputer) &key new-data)
  (with-slots (data-set missing-value missing-value-test replacers) instance
    (let ((transform-data (if (null new-data)
                              data-set new-data)))
      (mapcar #'(lambda (row)
                  (let ((col-counter -1))
                    (mapcar #'(lambda (item)
                                (incf col-counter)
                                (if (funcall missing-value-test item missing-value)
                                    (aref replacers col-counter)
                                  item))
                            row)))
              transform-data))))
