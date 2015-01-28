;;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :mlep)

(defclass k-nearest-neighbors ()
  ((k
    :initarg :k
    :initform 2
    :reader k
    :documentation "The number of neighbors to be taken into account.")
   (data-set 
    :initarg :data-set
    :initform nil
    :accessor data-set
    :documentation "The data-set that is already known. (`set-labels' go hand in hand with it.)")
   (set-labels
    :initarg :set-labels
    :initform nil
    :accessor set-labels
    :documentation "The labels for `data-set'.")
   (distance
    :initarg :distance
    :initform #'euclidian-distance
    :accessor distance
    :documentation "A distance measuring function.")
   (test-set
    :initarg :test-set
    :initform nil
    :accessor test-set
    :documentation "The data-set that has no labels and needs to be classified."))
  (:documentation "k-nearest-neighbors is a simple supervised clustering algorithm for a known number of clusters."))

(defun number-not-in-list (number list)
  (if (member number list)
      (number-not-in-list (1+ number) list)
    number))

(defun k-maxmin-idx (list &key (k 1) (key #'<))
  (let ((indices))
    (dotimes (i k (nreverse indices))
      (push (do* ((idx 0 (1+ idx))
                  (tmp-list list (cdr tmp-list))
                  (min-val (first tmp-list))
                  (min-idx (number-not-in-list 0 indices)))
                 ((endp tmp-list) min-idx)
              (when (and (funcall key (first tmp-list) min-val) (not (member idx indices)))
                (setf min-val (first tmp-list))
                (setf min-idx idx)))
            indices))))

(defun counter (list)
  (let ((result))
    (dolist (item list (nreverse result))
      (let ((already (assoc item result)))
        (if already
            (incf (rest already))
          (setf result (acons item 1 result)))))))

(defmethod run ((instance k-nearest-neighbors) &key)
  (with-slots (data-set set-labels test-set distance k) instance
    (let ((data-len (length data-set)))
      (when (/= data-len (length set-labels))
        (error "Each item in `data-set' has to have a label in `set-labels'. (At the moment they are of unequal length.)"))
      (when (< data-len k)
        (error "`k' is greater than the number of items in `data-set'."))
      (let ((result))
        (dolist (item test-set (nreverse result))
          (let ((distances))
            (dotimes (i (length data-set))
              (push (funcall distance item (nth i data-set)) distances))
            (let ((occurences
                   ; get the k smallest distances and count their labels
                   (counter 
                    (mapcar #'(lambda (n) (nth n set-labels))
                            (k-maxmin-idx (nreverse distances) :k k :key #'<)))))
              (push (first (nth (first (k-maxmin-idx (mapcar #'rest occurences)
                                                     :k 1 :key #'>))
                                occurences))
                    result))))))))