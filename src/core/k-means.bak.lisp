;;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :mlep)

(defclass k-means ()
  ((k
    :initarg :k
    :initform 2
    :reader k
    :documentation "The numner of groups/clusters to be determined.")
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
  (:documentation "k-means is a simple un-supervised clustering algorithm for a known number of clusters."))

; initialize means randomly
(defmethod initialize-instance :after ((instance k-means) &key)
  (with-slots (data-set k means) instance
    (when (or (null means) (\= (list-length means) k))
      (let ((ranges (get-column-ranges data-set)))
        (setf means
              (loop for i from 0 below k collect
                    (mapcar #'(lambda (from-to)
                                (apply #'random-from-to from-to))
                            ranges)))))))

; writer method for k of k-means. if k changes, the means have to be re-initialized
(defmethod (setf k) :after (k (instance k-means))
  (setf (means instance) nil)
  (format t "~&Re-initialize means...")
  (initialize-instance instance)
  k)

(defmethod run ((instance k-means) &key (learning-cycles 100))
  (with-slots (data-set k distance means) instance
    (flet ((mean-by-index (indices list)
             (mapcar #'mean
                     (transpose (mapcar #'(lambda (i) (nth i list))
                                        indices)))))
      (when (or (null means) (/= (list-length means) k))
        (format t "~&Re-initialize means...")
        (initialize-instance instance))
      (dotimes (i learning-cycles)
        (let* ((membership
                (classify instance))
               (membership-index
                (loop for i from 0 below k collect
                      (all-positions i membership))))
          (dotimes (i k)
            (unless (null (nth i membership-index))
              (setf (nth i means) (mean-by-index (nth i membership-index)
                                                 data-set)))))))
    means))

(defmethod classify ((instance k-means))
  (with-slots (data-set distance means) instance
    (mapcar #'(lambda (l) (position (apply #'min l) l))
            (mapcar #'(lambda (item)
                        (loop for i from 0 below (length means) collect
                              (funcall distance item (nth i means))))
                    data-set))))