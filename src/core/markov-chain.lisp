;;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :mlep)

(defclass markov-chain ()
  ((data-set
    :initarg :data-set
    :initform nil
    :accessor data-set
    :documentation "The data-set to be analyzed.")
   (order
    :initarg :order
    :initform 1
    :accessor order
    :documentation "The order of the markov chain.")
   (unique
    :initarg :unique
    :initform nil
    :reader unique
    :documentation "Unique values of data-set")
   (probabilities
    :initarg :probabilities
    :initform nil
    :accessor probabilities
    :documentation "A matrix/tensor with probabilities."))
  (:documentation "A Markov-Chain."))

(defun normalize-tensor (tensor)
  (let ((dim (array-dimensions tensor)))
    (if (/= (array-rank tensor) 1)
        (dotimes (i (first dim))
          (normalize-tensor (make-array (rest dim) :displaced-to tensor :displaced-index-offset (* i (reduce #'* (rest dim))))))
      (let ((sum (reduce #'+ tensor)))
        (when (not (zerop sum))
          (dotimes (i (length tensor))
            (setf (aref tensor i) (/ (aref tensor i) sum))))))
    tensor))

(defmethod run ((instance markov-chain) &key)
  (with-slots (data-set order unique probabilities) instance
    (when (consp data-set)
      (setf data-set (coerce data-set 'vector)))
    (setf unique (remove-duplicates data-set :test #'equal))
    (let ((indices))
      ; learn tensor
      (setf probabilities (make-array (make-list (1+ order) :initial-element (length unique)) :initial-element 0))
      (dotimes (i (length data-set))
        (push (position (aref data-set i) unique :test #'equal) indices)
        (when (>= i order)
          (incf (apply #'aref probabilities (reverse indices)))
          (setf indices (butlast indices)))) ; this is not best practice for lists...
      ; normalize it
      (setf probabilities (normalize-tensor probabilities)))
    probabilities))

; altern by DLO
(defun altern (boul prob)
  (let ((u (random 1.0)))
    (do* ((n 0 (1+ n))
          (som (nth 0 prob) (+ som (nth n prob))))
         ((< u som) (nth n boul)))))

(defmethod synthesize ((instance markov-chain) &key (start) (howmany 10))
  (with-slots (data-set order unique probabilities) instance
    (let ((result (make-array (+ howmany order) :element-type (array-element-type data-set)))
          (start-length order))
      (cond ((eq start 'random)
             (dotimes (i start-length)
               (setf (aref result i) (aref unique (random (length unique))))))
            ((null start)
             (let ((start-idx (random (- (length data-set) start-length 1))))
               (dotimes (i start-length)
                 (setf (aref result i) (aref data-set (+ i start-idx))))))
            ((consp start)
             (let ((i 0))
               (dolist (item start)
                 (setf (aref result i) item)
                 (incf i)))
             (setf start-length (length start)))
            ((vectorp start)
             (dotimes (i (length start))
               (setf (aref result i) (aref start i)))
             (setf start-length (length start)))
            (T (error "Cannot handle start value: ~a" start)))
      (let ((to-analyze (make-array order
                                    :element-type (array-element-type data-set)
                                    :displaced-to result
                                    :displaced-index-offset (- start-length order)
                                    :adjustable T)))
        (dotimes (i howmany result)
          (let ((this-probs (loop for idx from 0 below (first (array-dimensions probabilities))
                                  collect (apply #'aref probabilities (append (map 'list #'(lambda (x) (position x unique :test #'equal))
                                                                                      to-analyze) (list idx))))))
            (if (every #'zerop this-probs)
                (progn
                  (format t "Early abortion. All probs zero.")
                  (return (make-array (+ i start-length) :displaced-to result :element-type (array-element-type data-set))))
              (progn
                (setf (aref result (+ i start-length))
                      (altern
                       (coerce unique 'list)
                       this-probs))
            (adjust-array to-analyze
                          order
                          :element-type (array-element-type data-set)
                          :displaced-to result
                          :displaced-index-offset (- (+ i start-length 1) order))))))))))

(defmethod analyze ((instance markov-chain) input &key)
  (with-slots (order unique probabilities) instance
    (let* ((probs)
           (input (if (consp input) (coerce input 'vector) input))
           (start (make-array (1+ order) :displaced-to input :adjustable T :element-type (array-element-type input))))
      (if (some #'(lambda (item)
                    (not (position item unique)))
                input)
          0
        (dotimes (i (- (length input) order) (reduce #'* probs))
          (push (apply #'aref probabilities (map 'list #'(lambda (x) (position x unique :test #'equal))
                                                 start))
                probs)
          (adjust-array start (1+ order) :displaced-to input :displaced-index-offset i))))))