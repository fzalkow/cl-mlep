;;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :mlep)

(defun scalar-product (x1 x2)
  "Computes the scalar product (aka dot product aka inner product) of two vectors/lists."
  (reduce #'+ (map (when (vectorp x1) 'vector 'list)
                   #'(lambda (a b) (* a b)) x1 x2)))

(defun outer-product (x1 x2)
  "Computes the outer product (aka tensor product) of two vectors/lists `x1' and `x2'."
  (let* ((l1 (length x1))
         (l2 (length x2))
         (result (make-array (list l1 l2))))
    (dotimes (i l1 result)
      (dotimes (j l2)
        (setf (aref result i j) (* (elt x1 i) (elt x2 j)))))))

(defun transpose-array (array)
  "Transpose an 2-dimensional array."
  (let* ((m (array-dimension array 0))
         (n (array-dimension array 1))
         (new (make-array (list n m) :initial-element 0)))
    (dotimes (i m new)
      (dotimes (j n)
        (setf (aref new j i)
              (aref array i j))))))

(defun get-row-ranges-array (set &key (key #'identity))
  (let* ((len (first (array-dimensions set)))
         (range (make-array (list len 2) :initial-element 0)))
    (dotimes (i len range)
      (let ((key-row (funcall key (row-of-array set i))))
        (setf (aref range i 0) (reduce #'min key-row))
        (setf (aref range i 1) (reduce #'max key-row))))))

(defun map-pointwise (fn &rest arrays)
  "Calls `function' to one or more `arrays' pointwise."
  (let* ((dim (array-dimensions (elt arrays 0)))
         (result (make-array dim)))
    (dolist (array arrays)
      (check-type array array)
      (assert (equal (array-dimensions array) dim) (array)
        "All arrays must have same dimensions."))
    (labels ((helper (indices dimensions)
               (if dimensions
                   (loop for i below (car dimensions) do
                         (helper (cons i indices) (cdr dimensions)))
                 (setf (apply #'aref result indices)
                       (apply fn (mapcar #'(lambda (array)
                                             (apply #'aref array indices))
                                         arrays))))))
      (helper nil (reverse dim))
      result)))

(defun determinant (matrix)
  "Computes the determinant of `matrix' - which must a square matrix of rank 2.."
  (let ((dim (array-dimensions matrix)))
    (check-type matrix array)
    (assert (and (= (length dim) 2)
                 (= (first dim) (second dim)))
        (matrix) "Matrix must be a 2-dimensional square matrix")
    (determinant-helper matrix)))

(defun list-to-2d-array (list &key (element-type t))
  "Converts a list of list to a 2-dimensional array."
  (unless (eq element-type t)
    (setf list (loop for row in list collect
                     (loop for item in row collect
                           (coerce item element-type)))))
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list
              :element-type element-type))

(defun list-to-vector (list &key (element-type t))
  "Converts a list to a vector."
  (unless (eq element-type t)
    (setf list (loop for item in list collect
                     (coerce item element-type))))
  (make-array (length list)
              :initial-contents list
              :element-type element-type))

(defun col-of-array (array col)
  "Return column of 2d array as vector"
  (let* ((len (first (array-dimensions array)))
         (new-array (make-array len
                                :element-type (array-element-type array))))
    (dotimes (i len new-array)
      (setf (aref new-array i)
            (aref array i col)))))

(defun row-of-array (array row)
  "Return row of 2d array as vector"
  (let* ((len (second (array-dimensions array)))
         (new-array (make-array len
                                :element-type (array-element-type array))))
    (dotimes (i len new-array)
      (setf (aref new-array i)
            (aref array row i)))))

(defun (setf row-of-array) (vector array row)
  (let ((row-len (second (array-dimensions array))))
    (dotimes (i row-len array)
      (setf (aref array row i) (aref vector i)))))

(defun (setf col-of-array) (vector array col)
  (let ((col-len (first (array-dimensions array))))
    (dotimes (i col-len array)
      (setf (aref array i col) (aref vector i)))))

; I don't know much about matrix algebra... I just ported the code from
; http://code.activestate.com/recipes/578108-determinant-of-matrix-of-any-order/
; and it seems to work
(defun determinant-helper (matrix)
  (let ((len (second (array-dimensions matrix))))
    (if (> len 2)
        (let ((i 1)
              (sum 0))
          (dotimes (tt len)
            (let ((d (make-hash-table)))
              (dotimes-fromto (tt1 1 len)
                (setf (gethash tt1 d) nil)
                (dotimes (m len)
                  (when (/= m tt)
                    (setf (gethash tt1 d) (cons (aref matrix tt1 m) (gethash tt1 d))))))
              (let ((l1 (mapcar #'reverse (loop for val being the hash-value in d collect val))))
                (incf sum (* i (aref matrix 0 tt)
                             (determinant-helper (make-array (list (length l1)
                                                                   (length (first l1)))
                                                             :initial-contents l1))))
                (setf i (* i -1)))))
          sum)
      (- (* (aref matrix 0 0) (aref matrix 1 1))
         (* (aref matrix 0 1) (aref matrix 1 0))))))

(defun vector-to-2d-col (vec)
  (let ((result (make-array (list 1 (first (array-dimensions vec)))
                            :element-type (array-element-type vec))))
    (dotimes (i (first (array-dimensions vec)) result)
      (setf (aref result 0 i) (aref vec i)))))

(defun 2d-col-to-vector (array)
  (let ((result (make-array (second (array-dimensions array))
                            :element-type (array-element-type array))))
    (dotimes (i (second (array-dimensions array)) result)
      (setf (aref result i) (aref array 0 i)))))
                

; the three functions `multiply-matrices', `matrix-identity' and `matrix-expt' are just copied from
; http://rosettacode.org/wiki/Matrix-exponentiation_operator#Common_Lisp
(defun multiply-matrices (matrix-0 matrix-1)
  "Takes two 2D arrays and returns their product, or an error if they cannot be multiplied"
  (let* ((m0-dims (array-dimensions matrix-0))
         (m1-dims (array-dimensions matrix-1))
         (m0-dim (length m0-dims))
         (m1-dim (length m1-dims)))
    (if (or (/= 2 m0-dim) (/= 2 m1-dim))
        (error "Array given not a matrix")
        (let ((m0-rows (car m0-dims))
              (m0-cols (cadr m0-dims))
              (m1-rows (car m1-dims))
              (m1-cols (cadr m1-dims)))
          (if (/= m0-cols m1-rows)
              (error "Incompatible dimensions")
              (do ((rarr (make-array (list m0-rows m1-cols)
                                     :initial-element 0) rarr)
                   (n 0 (if (= n (1- m0-cols)) 0 (1+ n)))
                   (cc 0 (if (= n (1- m0-cols))
                             (if (/= cc (1- m1-cols)) 
                                 (1+ cc) 0) cc))
                   (cr 0 (if (and (= (1- m0-cols) n) 
                                  (= (1- m1-cols) cc))
                             (1+ cr)
                             cr)))
                  ((= cr m0-rows) rarr)
                (setf (aref rarr cr cc)
                      (+ (aref rarr cr cc)
                         (* (aref matrix-0 cr n)
                            (aref matrix-1 n cc))))))))))
 
(defun matrix-identity (dim &key (element-type t))
  "Creates a new identity matrix of size dim*dim"
  (do ((rarr (make-array (list dim dim)
                         :initial-element (coerce 0 element-type)
                         :element-type element-type) rarr)
       (n 0 (1+ n)))
      ((= n dim) rarr)
    (setf (aref rarr n n) (coerce 1 element-type))))
 
(defun matrix-expt (matrix exp)
  "Takes the first argument (a matrix) and multiplies it by itself exp times"
  (let* ((m-dims (array-dimensions matrix))
         (m-rows (car m-dims))
         (m-cols (cadr m-dims)))
    (cond
      ((/= m-rows m-cols) (error "Non-square matrix"))
      ((zerop exp) (matrix-identity m-rows))
      ((= 1 exp) (do ((rarr (make-array (list m-rows m-cols)) rarr)
                      (cc 0 (if (= cc (1- m-cols))
                                0
                                (1+ cc)))
                      (cr 0 (if (= cc (1- m-cols))
                                (1+ cr)
                                cr)))
                     ((= cr m-rows) rarr)
                   (setf (aref rarr cr cc) (aref matrix cr cc))))
      ((zerop (mod exp 2)) (let ((me2 (matrix-expt matrix (/ exp 2))))
                             (multiply-matrices me2 me2)))
      (t (let ((me2 (matrix-expt matrix (/ (1- exp) 2))))
           (multiply-matrices matrix (multiply-matrices me2 me2)))))))