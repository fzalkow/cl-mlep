;;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :mlep)

(defclass naive-bayes ()
  ((data-set 
    :initarg :data-set
    :initform nil
    :accessor data-set
    :documentation "The data-set that is already known. (@code{set-labels} go hand in hand with it.)")
   (set-labels
    :initarg :set-labels
    :initform nil
    :accessor set-labels
    :documentation "The labels for @code{data-set}.")
   (test-set
    :initarg :test-set
    :initform nil
    :accessor test-set
    :documentation "The data-set that has no labels and needs to be classified.")

   (possible-data-values
    :initarg :possible-data-values
    :initform nil
    :accessor possible-data-values
    :documentation "All possible values occurring in each attribute of @code{data-set}. To be pre-computed.")
   (all-labels
    :initarg :all-labels
    :initform nil
    :accessor all-labels
    :documentation "All given data-labels. To be pre-computed.")
   (label-count
    :initarg :label-count
    :initform nil
    :accessor label-count
    :documentation "Counting of each item of a label. To be pre-computed.")
   (prior-probabilities
    :initarg :prior-probabilities
    :initform nil
    :accessor prior-probabilities
    :documentation "The prior-probabilities of each class. To be pre-computed.")
   (likelihoods
    :initarg likelihoods
    :initform nil
    :accessor likelihoods
    :documentation "The likelihoods of a feature attribute given a class. To be pre-computed."))
  (:documentation "Naive-bayes takes an probabilistic approach for a simple supervised clustering."))

(defmethod initialize ((instance naive-bayes) &key)
  "Pre-computes: `all-labels', `label-count', `possible-data-values', `prior-probabilities', `likelihoods'"
  (with-slots (data-set set-labels all-labels label-count possible-data-values prior-probabilities likelihoods) instance
    ; all-labels
    (setf all-labels (sort (remove-duplicates set-labels) #'<))
    ; label-count
    (setf label-count (mapcar #'(lambda (label) (cons label 0)) all-labels))
    (dolist (label set-labels)
      (incf (rest (find label label-count :key #'first))))
    ; possible-data-values
    (setf possible-data-values (compute-possible-data-values data-set))
    ; prior-probabilities
    (setf prior-probabilities (compute-prior-probabilities set-labels label-count))
    ; likelihoods
    (setf likelihoods (compute-likelihoods data-set set-labels all-labels label-count possible-data-values))))

(defmethod initialize-instance :after ((instance naive-bayes) &key)
  (initialize instance))

(defmethod (setf data-set) :after (data-set (instance naive-bayes))
  (initialize instance))

(defmethod (setf set-labels) :after (set-labels (instance naive-bayes))
  (initialize instance))

(defun compute-possible-data-values (data-set)
  (mapcar #'(lambda (row) (sort (remove-duplicates row) #'<))
          (transpose data-set)))

(defun compute-prior-probabilities (data-labels label-count)
  (let ((len (length data-labels)))
    (mapcar #'(lambda (c) (cons (first c) (/ (rest c) len)))
            label-count)))

; good explaination: http://stackoverflow.com/a/20556654/2812618
(defun compute-likelihoods (set data-labels all-labels label-count possible-data-values)
  (let ((all-attributes)
        (likelihoods))
    (do-lists ((pos values) ((range 0 (length (first set))) possible-data-values))
      (dolist (val values)
        (push (cons pos val) all-attributes)))
    ; likelihoods is a list '((label (attribute-position . attribute-value) . likelihood) ...)
    (dolist (label all-labels)
      (dolist (attribute all-attributes)
        (push (cons label (cons attribute 0)) likelihoods)))
    ; go through all data items and count for getting the likelihoods
    (do-lists ((item item-label) (set data-labels))
      (dolist (likelihood likelihoods)
        (when (and (eql item-label (first likelihood))
                   (eql (nth (first (second likelihood)) item) (rest (second likelihood))))
          (incf (cddr likelihood)))))
    ; normalize counter to get the real likelihood
    (dolist (likelihood likelihoods (nreverse likelihoods))
      (setf (cddr likelihood)
            (/ (cddr likelihood) (rest (find (first likelihood) label-count :key #'first)))))))

    
(defmethod run ((instance naive-bayes) &key)
  (with-slots (data-set set-labels test-set all-labels prior-probabilities likelihoods) instance
    (when (/= (length data-set) (length set-labels))
      (error "Each item in `data-set' has to have a label in `set-labels'. (At the moment they are of unequal length.)"))
    (let ((result))
      (dolist (item test-set (nreverse result))
        (let ((probabilities))
          (dolist (label all-labels)
            (let ((tmp-prob (rest (find label prior-probabilities :key #'first))))
              (dotimes (i (length item))
                (let ((eish (cddar (member-if #'(lambda (lik)
                                                  (and (eql (first lik)
                                                            label)
                                                       (equal (second lik)
                                                              (cons i (nth i item)))))
                                              likelihoods))))
                  (setf tmp-prob (* tmp-prob (if eish eish 0)))))
              (push tmp-prob probabilities)))
          (if (every #'zerop probabilities)
              (push nil result)
            (push (nth (max-arg (nreverse probabilities)) all-labels) result)))))))
          