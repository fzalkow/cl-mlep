;;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :mlep)

(defclass perceptron ()
  ((data-set
    :initarg :data-set
    :initform nil
    :accessor data-set
    :documentation "The data-set to be analyzed.")
   (set-labels
    :initarg :set-labels
    :initform nil
    :accessor set-labels
    :documentation "The output-values for @code{data-set}.")
   (weights
    :initarg :weights
    :initform nil
    :accessor weights
    :documentation "The weights from input to output values.")
   (max-weight-init-value
    :initarg :max-weight-init-value
    :initform 0.1d0
    :accessor max-weight-init-value
    :documentation "The maximum value for initializing the weights.")
   (activation-function
    :initarg :activation-function
    :initform #'step-function
    :accessor activation-function
    :documentation "The activation function, usually a Heaviside step function.")
   (learning-rate
    :initarg :learning-rate
    :initform 0.2d0
    :accessor learning-rate
    :documentation "The learning rate."))
  (:documentation "A perceptron is a very simple neuron model and turns out to be a linear classificator."))

(defmethod initialize-instance :after ((instance perceptron) &key)
  (initialize instance))

(defmethod (setf data-set) :after (data-set (instance perceptron))
  (initialize instance))

(defmethod (setf set-labels) :after (set-labels (instance perceptron))
  (initialize instance))

(defmethod (setf max-weight-init-value) :after (max-weight-init-value (instance perceptron))
  (initialize instance))

(defmethod initialize ((instance perceptron) &key)
  (with-slots (data-set max-weight-init-value weights) instance
    (let* ((len-input (length (nth 0 data-set)))
           (tmp-weights (make-array (1+ len-input) :element-type 'double-float :initial-element 0.0d0)))
      (dotimes (i (1+ len-input))
        (setf (aref tmp-weights i) (random max-weight-init-value)))
      (setf weights tmp-weights))))

(defun get-actual-output (activation-function weights input)
  (funcall activation-function (+ (aref weights 0)
                                  (scalar-product (subseq weights 1) input))))

(defun get-overall-error (all-input desired activation-function weights)
  (let ((actual-outputs))
    (dolist (input all-input)
      (push (get-actual-output activation-function weights input) actual-outputs))
    (/ (reduce #'+ (mapcar #'(lambda (a b) (abs (- a b))) (nreverse actual-outputs) desired)) (length all-input))))

(defmethod run ((instance perceptron) &key (threshold 0.1))
  (with-slots (data-set set-labels weights activation-function learning-rate) instance
    (let ((data-len (length data-set)))
      (when (/= data-len (length set-labels))
        (error "Each item in `data-set' has to have a label in `set-labels'. (At the moment they are of unequal length.)"))
      (do ((overall-error (get-overall-error data-set set-labels activation-function weights)
                          (get-overall-error data-set set-labels activation-function weights)))
          ((< overall-error threshold) overall-error)
        (do-lists ((input output i) (data-set set-labels (range 0 data-len)))
          (let ((actual-output (get-actual-output activation-function weights input)))
            (setf weights (map 'vector #'(lambda (w in) (+ w (* learning-rate (- output actual-output) in))) weights (concatenate 'vector #(1.0d0) input)))
            (values)))))))

(defmethod classify ((instance perceptron) &key new-data-set)
  (with-slots (data-set weights activation-function) instance
    (let ((set-to-analyze (if new-data-set
                              (if (consp new-data-set)
                                  (list-to-2d-array new-data-set)
                                new-data-set)
                            (if (consp data-set)
                                (list-to-2d-array data-set)
                              data-set))))
      (loop for i from 0 below (first (array-dimensions set-to-analyze))
            for row = (row-of-array set-to-analyze i)
            collect (get-actual-output activation-function weights row)))))
