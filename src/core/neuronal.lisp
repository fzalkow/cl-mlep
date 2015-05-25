;;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :mlep)

(defclass neuronal-network ()
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
   (net-structure
    :initarg :net-structure
    :initform nil
    :accessor net-structure
    :documentation "The net-structure of the net. (Neurons per layer.)")
   (weights
    :initarg :weights
    :initform nil
    :accessor weights
    :documentation "The weights from input to output values.")
   (output-net
    :initarg :output-net
    :initform nil
    :accessor output-net
    :documentation "The output of all neurons of the network.")
   (output-net-before-activation
    :initarg :output-net-before-activation
    :initform nil
    :accessor output-net-before-activation
    :documentation "The output of all neurons of the network before the activation function was applied.")
   (weight-init-range
    :initarg :weight-init-range
    :initform '(-0.1d0 0.1d0)
    :accessor weight-init-range
    :documentation "The maximum range for initializing the weights.")
   (activation-function
    :initarg :activation-function
    :initform #'tanh
    :accessor activation-function
    :documentation "The activation function, usually a Heaviside step function.")
   (learning-rate
    :initarg :learning-rate
    :initform 0.2d0
    :accessor learning-rate
    :documentation "The learning rate."))
  (:documentation "A fully-connected Feed-Forward Multi-Layer Perceptron."))

(defmethod initialize-instance :after ((instance neuronal-network) &key)
  (initialize instance))

(defmethod (setf data-set) :after (data-set (instance neuronal-network))
  (initialize instance))

(defmethod (setf set-labels) :after (set-labels (instance neuronal-network))
  (initialize instance))

(defmethod (setf weight-init-range) :after (weight-init-range (instance neuronal-network))
  (initialize instance))

(defmethod (setf net-structure) :after (net-structure (instance neuronal-network))
  (initialize instance))

(defmethod initialize ((instance neuronal-network) &key)
  (with-slots (net-structure output-net output-net-before-activation weight-init-range weights) instance
    (setf weights (net-structure-to-weights net-structure weight-init-range))
    (setf output-net (net-structure-to-array net-structure 1.0d0))
    (setf output-net-before-activation (net-structure-to-array net-structure 1.0d0))))

(defun net-structure-to-weights (net-structure &optional weight-init-range)
  (let* ((len-1 (1- (length net-structure)))
         (weights (make-array len-1 :element-type 'array)))
    ; all layers except last one have bias unit
    (dotimes-fromto (i 1 len-1)
      (setf (aref weights (1- i))
            (make-array (list (1+ (nth (1- i) net-structure))
                              (1+ (nth i net-structure)))
                        ::element-type 'double-float :initial-element 0.0d0)))
    ; last layer does not have a bias unit
    (setf (aref weights (1- len-1))
          (make-array (list (1+ (nth (1- len-1) net-structure))
                            (nth len-1 net-structure))
                      ::element-type 'double-float :initial-element 0.0d0))
    ; initialize weights
    (when weight-init-range
      (dotimes (idx len-1)
        (dotimes (i (first (array-dimensions (aref weights idx))))
          (dotimes (j (second (array-dimensions (aref weights idx))))
            (setf (aref (aref weights idx) i j) (coerce (apply #'random-from-to weight-init-range) 'double-float))))))
    weights))

(defun net-structure-to-array (net-structure &optional (value 0.0d0))
  (let ((bias (make-array (length net-structure) :element-type 'array)))
    (dolist-with-index (idx item net-structure bias)
      (setf (aref bias idx)
            (make-array item :element-type 'double-float :initial-element value)))))


         
(defmethod forward ((instance neuronal-network) &key input)
  (with-slots (weights output-net output-net-before-activation activation-function) instance
    (let ((tmp (make-array (1+ (first (array-dimensions input)))
                           :initial-element 1.0d0)))  
      ; initialize input (with bias)
      (dotimes (i (first (array-dimensions input)))
        (setf (aref tmp i) (aref input i)))
      (setf (aref output-net 0) tmp)
      (setf (aref output-net-before-activation 0) tmp)

      (dotimes-fromto (layer 1 (first (array-dimensions output-net))
                             (aref output-net (1- (first (array-dimensions output-net)))))

        (let ((input-in-inet (2d-col-to-vector
                              (multiply-matrices (vector-to-2d-col (aref output-net (1- layer)))
                                                 (aref weights (1- layer))))))
          (setf (aref output-net-before-activation layer) input-in-inet)
          (setf (aref output-net layer)
                (map 'vector activation-function input-in-inet)))))))

(defmethod backprop ((instance neuronal-network) input wanted)
  (with-slots (weights output-net output-net-before-activation activation-function learning-rate) instance
    (let* ((len (first (array-dimensions output-net)))
           (out (forward instance :input input))
           (error (map 'vector #'- wanted out))
           (delta (make-array (1- len) :element-type 'array)))
      ; compute deltas
      (setf (aref delta (- len 2))
            (map 'vector #'* error (map 'vector (diff activation-function) 
                                        (aref output-net-before-activation (1- len)))))
      (loop for layer from (- len 2) downto 1 do
            (setf (aref delta (1- layer))
                  (map 'vector #'*
                       (2d-col-to-vector
                        (multiply-matrices (vector-to-2d-col (aref delta layer))
                                           (transpose (aref weights layer))))
                       (map 'vector (diff activation-function) (aref output-net-before-activation layer)))))
      ; adjust weights
      (dotimes (i (first (array-dimensions weights)))
        (let ((layer (vector-to-2d-col (aref output-net i)))
              (delt (vector-to-2d-col (aref delta i))))
          (setf  (aref weights i) 
                 (map-pointwise #'+
                                (aref weights i)
                                (map-pointwise #'(lambda (x) (* x learning-rate))
                                               (multiply-matrices (transpose layer) delt)))))))))

(defmethod classify ((instance neuronal-network) &key new-data-set verbose)
  (with-slots (data-set set-labels) instance
    (let* ((set-to-analyze (if new-data-set
                               (if (consp new-data-set)
                                   (list-to-2d-array new-data-set)
                                 new-data-set)
                             (if (consp data-set)
                                 (list-to-2d-array data-set)
                               data-set)))
           (result
            (loop for i from 0 below (first (array-dimensions set-to-analyze))
                  for row = (row-of-array set-to-analyze i)
                  for tmp-result = (forward instance :input row)
                  collect tmp-result
                  when verbose
                  do (format t "~&~a -> ~a ~{(target: ~a)~}"
                             row
                             tmp-result
                             (unless new-data-set
                               (list (nth i set-labels)))))))
      result)))

(defmethod run ((instance neuronal-network) &key (epochs 100))
  (with-slots (data-set set-labels) instance
    (let ((data-set (list-to-2d-array data-set))
          (set-labels (list-to-2d-array set-labels)))
      (dotimes (i epochs)
        (let ((idxs (shuffle (range 0 (first (array-dimensions data-set))))))
          (dolist (idx idxs)
            (backprop instance (row-of-array data-set idx) (row-of-array set-labels idx)))))))
  (values))
    
                                
      
