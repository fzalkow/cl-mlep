;;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :mlep)

(defun get-row-ranges (set &key (key #'identity))
  "SET is a two-dimensional list or array. Gets the minimal and maximal value of each row
(i.e. sublist) of SET."
  (if (consp set)
      (get-row-ranges-list set :key key)
    (get-row-ranges-array set :key key)))


(defun get-row-ranges-list (set &key (key #'identity))
  (mapcar #'(lambda (row)
	      (let ((key-row (funcall key row)))
		(list (reduce #'min key-row)
		      (reduce #'max key-row))))
	  set))

(defun get-column-ranges (set &key (key #'identity))
  "SET is a two-dimensional list or array. Gets the minmal and maximal value of each
column of SET. Column means the corresponding nth values of each sublist."
  (get-row-ranges (transpose set) :key key))

(defun all-positions (item list &key (test #'eql) (key #'identity))
  "Gets the indices of all elements in LIST that safisfy the TEST."
  (let ((positions))
    (dotimes (i (length list) (nreverse positions))
      (when (funcall test item (funcall key (nth i list)))
        (push i positions)))))

(defun mean (l)
  "Gets the mean of `l'."
  (/ (reduce #'+ l) (length l)))

(defun max-arg (l)
  "Gets the index of the maximum value of `l'."
  (let ((tmp-arg (first l))
        (tmp-idx 0))
    (do-lists ((arg idx) (l (range 0 (list-length l))) tmp-idx)
      (when (> arg tmp-arg)
        (setf tmp-arg arg)
        (setf tmp-idx idx)))))

(defun min-arg (l)
  "Gets the index of the minimum value of `l'."
  (let ((tmp-arg (first l))
        (tmp-idx 0))
    (do-lists ((arg idx) (l (range 0 (list-length l))) tmp-idx)
      (when (< arg tmp-arg)
        (setf tmp-arg arg)
        (setf tmp-idx idx)))))

(defun transpose-list (l)
  "Transpose a list of lists."
  (do ((tmp-l l (mapcar #'rest tmp-l))
       (result nil (cons (mapcar #'first tmp-l) result)))
      ((endp (first tmp-l)) (nreverse result))))

; the solution is common:
;(defun transpose-list (l)
;  (apply #'mapcar #'list l))
; but it holds only if l has max length of call-arguments-limit
; sadly the version used here is a bit slower than the common one,
; but it is not restricted by call-arguments-limit 

(defun transpose (x)
  "Transpose 2-dimensional list or array."
  (cond ((consp x)
         (transpose-list x))
        ((arrayp x)
         (transpose-array x))
        (T NIL)))

(defun shuffle (x)
  "Shuffle a list."
  (flet ((remove-idx (l idx)
           (remove-if (constantly t) l :start idx :count 1)))
    (let* ((len (length x))
           (result)
           (idxs (range 0 len)))
      (dotimes (i len result)
        (let ((idx (random len)))
          (push (nth (nth idx idxs) x) result)
          (setf idxs (remove-idx idxs idx))
          (decf len))))))

(defun most-frequent-value (x &key (test #'eql))
  "Get the most frequent value of a sequence x."
  (let ((value nil)
        (times 0)
        (counter (make-hash-table :test test))
        (tmp-times))
    (map nil #'(lambda (item)
                 (if (gethash item counter)
                     (setf tmp-times (incf (gethash item counter)))
                   (setf (gethash item counter) 1
                         tmp-times 1))
                 (when (> tmp-times times)
                   (setf times tmp-times
                         value item)))
         x)
    value))