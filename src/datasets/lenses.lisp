;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;; Lenses Data Set by J. Cendrowska (1987)
;;; https://archive.ics.uci.edu/ml/datasets/Lenses
;;; Attribute Information:
;;;    1. age of the patient (1 = young, 2 = pre-presbyopic, 3 = presbyopic)
;;;    2. spectacle prescription (1 = myope, 2 = hypermetrope)
;;;    3. astigmatic (1 = no, 2 = yes)
;;;    4. tear production rate (1 = reduced, 2 = normal)
;;;    5. class (1 = the patient should be fitted with hard contact lenses,
;;;              2 = the patient should be fitted with soft contact lenses,
;;;              3 = the patient should not be fitted with contact lenses)
;;; converted to CL list by Frank Zalkow, 2014

(in-package :mlep)

(defparameter *lenses*
  '((1 1 1 1 3)
    (1 1 1 2 2)
    (1 1 2 1 3)
    (1 1 2 2 1)
    (1 2 1 1 3)
    (1 2 1 2 2)
    (1 2 2 1 3)
    (1 2 2 2 1)
    (2 1 1 1 3)
    (2 1 1 2 2)
    (2 1 2 1 3)
    (2 1 2 2 1)
    (2 2 1 1 3)
    (2 2 1 2 2)
    (2 2 2 1 3)
    (2 2 2 2 3)
    (3 1 1 1 3)
    (3 1 1 2 3)
    (3 1 2 1 3)
    (3 1 2 2 1)
    (3 2 1 1 3)
    (3 2 1 2 2)
    (3 2 2 1 3)
    (3 2 2 2 3))
  "Lenses Data Set by J. Cendrowska (1987)

@a[https://archive.ics.uci.edu/ml/datasets/Lenses]{https://archive.ics.uci.edu/ml/datasets/Lenses}

Attribute Information:

@itemize{
@item{1. age of the patient (1 = young, 2 = pre-presbyopic, 3 = presbyopic)}
@item{2. spectacle prescription (1 = myope, 2 = hypermetrope)}
@item{3. astigmatic (1 = no, 2 = yes)}
@item{4. tear production rate (1 = reduced, 2 = normal)}
@item{5. class @itemize{@item{1 = the patient should be fitted with hard contact lenses,}
             @item{2 = the patient should be fitted with soft contact lenses,}
             @item{3 = the patient should not be fitted with contact lenses.}}}}")