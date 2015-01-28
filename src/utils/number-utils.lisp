;;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :mlep)

(defun random-from-to (from to &key (state *random-state*))
  "@arg[from]{the lower bound (inclusive)}
@arg[to]{the upper bound (exclusive)}
@arg[state]{a random state object containing information used by the pseudo-random number generator}
@return{a random number}
Gives a random number in certain range."
  (when (and (rationalp from) (not (integerp from)))
    (setf from (float from)))
  (when (and (rationalp to) (not (integerp to)))
    (setf to (float to)))
  (+ (random (- to from) state) from))

(defun range (from to &optional (step 1))
  "Gives a list with all numbers from `FROM' to (exclusive) `TO'."
  (if (< from to)
      (loop for x from from below to by step collect x)
    (loop for x from from above to by step collect x)))

(defun square (n)
  "Squares number N."
  (* n n))

(defun euclidian-distance (p1 p2)
  "Gives the euclidian distance between to points of arbitrary dimension."
  (sqrt (reduce #'+ (map 'vector #'square (map 'vector #'- p1 p2)))))