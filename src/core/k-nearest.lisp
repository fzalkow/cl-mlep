;;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :mlep)

(defclass k-nearest-neighbors ()
  ((k
    :initarg :k
    :initform 2
    :accessor k
    :documentation "The number of neighbors to be taken into account.")
   (data-set 
    :initarg :data-set
    :initform nil
    :accessor data-set
    :documentation "The data-set that is already known. (@code{set-labels} go hand in hand with it.)")
   (set-labels
    :initarg :set-labels
    :initform nil
    :accessor set-labels
    :documentation "The labels for @code{data-set}.")
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

#|
; old version for working with list structures

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
|#

(defun k-maxmin-idx (vector &key (k 1) (key #'<))
  "Get the index of k max/min values in vector."
  (let ((bak-vector (make-array (length vector) :initial-contents vector))
        (sorted-vector (sort vector key))
        (idx (make-array k)))
    (dotimes (i k idx)
      (setf (aref idx i)
            (position (aref sorted-vector i) bak-vector)))))

(defun histogram (vector)
  "Count all occurrences in vector. Returns list of all unique items in vector and their frequencies."
  (let* ((names (remove-duplicates vector))
         (counter (make-array (length names) :initial-element 0)))
    (dotimes (i (length vector) (list names counter))
      (incf (aref counter
                  (position (aref vector i)
                            names))))))



(defmethod run ((instance k-nearest-neighbors) &key)
  (with-slots (data-set set-labels test-set distance k) instance
    ; checks and conversions
    
    (when (consp data-set)
      (setf data-set (list-to-2d-array data-set)))
    (when (consp set-labels)
      (setf set-labels (list-to-vector set-labels)))
    (when (consp test-set)
      (setf test-set (list-to-2d-array test-set)))

    (let ((data-len (first (array-dimensions data-set))))

      (when (/= data-len (length set-labels))
        (error "Each item in `data-set' has to have a label in `set-labels'. (At the moment they are of unequal length.)"))
      (when (< data-len k)
        (error "`k' is greater than the number of items in `data-set'."))
      
      (labels ((classify-data-item (data-point)
                 ; get distances
                 (let ((distances (make-array data-len)))
                   (dotimes (i data-len)
                     (setf (aref distances i)
                           (funcall distance (row-of-array data-set i)
                                    data-point)))
                     ; get nearest labels
                   (let* ((nearest-indices (k-maxmin-idx distances))
                          (nearest-labels (map 'vector
                                               #'(lambda (x) (aref set-labels x))
                                               nearest-indices)))
                       ; from them, get most common label
                     (let* ((counter (histogram nearest-labels))
                            (names (first counter))
                            (times (second counter)))
                       (aref names
                             (position (reduce #'max times)
                                       times)))))))
        (let* ((test-len (first (array-dimensions test-set)))
              (result (make-array test-len)))
          (dotimes (i test-len result)
            (setf (aref result i)
                  (classify-data-item (row-of-array test-set i)))))))))