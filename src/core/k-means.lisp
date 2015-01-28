;;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :mlep)

(defclass k-means ()
  ((k
    :initarg :k
    :initform 2
    :reader k
    :documentation "The number of groups/clusters to be determined.")
   (data-set
    :initarg :data-set
    :initform nil
    :accessor data-set
    :documentation "The data-set to be analyzed.")
   (distance
    :initarg :distance
    :initform #'euclidian-distance
    :accessor distance
    :documentation "A distance measuring function.")
   (means
    :initarg :means
    :initform nil
    :accessor means
    :documentation "The means of the data points."))
  (:documentation "k-means is a simple unsupervised clustering algorithm for a known number of clusters."))

; initialize means randomly
(defmethod initialize-instance :after ((instance k-means) &key)
  (with-slots (data-set k means) instance
    (when (consp data-set)
      (setf data-set (list-to-2d-array data-set)))
    (when (or (null means) (\= (first (array-dimensions means)) k))
      (let ((ranges (get-column-ranges data-set)))
        (setf means
              (loop with new-means = (make-array (list k (second (array-dimensions data-set))))
                    for i from 0 below k
                    do (loop for j from 0 below (second (array-dimensions data-set))
                             do (setf (aref new-means i j)
                                      (apply #'random-from-to (coerce (row-of-array ranges j) 'list))))
                    finally (return new-means)))))))
                                     
; writer method for k of k-means. if k changes, the means have to be re-initialized
(defmethod (setf k) :after (k (instance k-means))
  (setf (means instance) nil)
  (format t "~&Re-initialize means...")
  (initialize-instance instance)
  k)

(defmethod run ((instance k-means) &key (epochs 100))
  (with-slots (data-set k distance means) instance
    (flet ((mean-by-index (indices 2d-array)
             (mapcar #'mean
                     (transpose (mapcar #'(lambda (i)
                                            (coerce (row-of-array 2d-array i) 'list))
                                        indices)))))
      (when (consp data-set)
        (setf data-set (list-to-2d-array data-set)))
      (when (or (null means) (\= (first (array-dimensions means)) k))
        (format t "~&Re-initialize means...")
        (initialize-instance instance))
      (dotimes (i epochs)
        (let* ((membership
                (classify instance))
               (membership-index
                (loop for i from 0 below k collect
                      (all-positions i membership))))
          (dotimes (i k)
            (unless (null (nth i membership-index))
              (setf (row-of-array means i) (coerce (mean-by-index (nth i membership-index)
                                                                  data-set)
                                                   'vector)))))))
    means))


(defmethod classify ((instance k-means) &key new-data-set)
  (with-slots (data-set distance means) instance
    (let ((set-to-analyze (if new-data-set
                              (if (consp new-data-set)
                                  (list-to-2d-array new-data-set)
                                new-data-set)
                            (if (consp data-set)
                                (list-to-2d-array data-set)
                              data-set))))
      (mapcar #'(lambda (l) (position (apply #'min l) l))
              (loop for i from 0 below (first (array-dimensions set-to-analyze))
                    for row = (row-of-array set-to-analyze i)
                    collect (loop for j from 0 below (first (array-dimensions means))
                                  collect (funcall distance row (coerce (row-of-array means j) 'list))))))))