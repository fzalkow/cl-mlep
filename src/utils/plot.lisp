;;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :mlep)

(defun plot-values (vals &key (height 20) (char #\x))
  "@arg[vals]{a sequence of numbers to be plotted}
@arg[height]{the height in characters used for the plot}
@arg[char]{the character used for printing}
@return{nothing}
Plot the values of @code{vals} successively."
  (let* ((max (reduce #'max vals))
         (min (reduce #'min vals))
         (scaled-vals (map 'vector #'(lambda (x)
                                       (round (* (- x min) (/ height (- max min)))))
                           vals)))
    (dotimes (j (1+ height))
      (dotimes (i (first (array-dimensions scaled-vals)))
      
        (if (= (aref scaled-vals i) (- height j))
            (princ char)
          (princ #\Space)))
      (terpri))
    (format t "~&val: [~a, ~a]" min max))
  (values))
    

(defun plot-points (vals &key (height 20) (width 80) (char #\x))
    "@arg[vals]{a list of list with x/y-points or a 2d-array -- @code{((x1 y1) ... (xn yn))} or @code{#2a((x1 y1) ... (xn yn))}}
@arg[height]{the height in characters used for the plot}
@arg[width]{the width in characters used for the plot}
@arg[char]{the character used for printing}
@return{nothing}
Plotting points with x/y-coordinates."
  (let* ((array (cond ((consp vals)
                       (list-to-2d-array vals :element-type 'double-float))
                      ((arrayp vals) vals)
                      (T (error "`vals' must be list or 2d array."))))
         (xvals (col-of-array array 0))
         (yvals (col-of-array array 1))
         (xmax (reduce #'max xvals))
         (ymax (reduce #'max yvals))
         (xmin (reduce #'min xvals))
         (ymin (reduce #'min yvals))
         (items (first (array-dimensions array)))
         (plot-idxs (make-hash-table :size items :test #'equal)))
    (dotimes (i items)
      ; scale x value
      (setf (gethash (list (round (* (- (aref array i 0) xmin) (/ width (- xmax xmin))))
                           (- height (round (* (- (aref array i 1) ymin) (/ height (- ymax ymin))))))
                     plot-idxs)
                     t))
    (dotimes (j (1+ height))
      (dotimes (i (1+ width))
        (if (gethash (list i j) plot-idxs)
            (princ char)
          (princ #\Space)))
      (terpri))
    (format t "~&x: [~a, ~a]~%y: [~a, ~a]" xmin xmax ymin ymax)
  (values)))


