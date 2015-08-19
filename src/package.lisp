;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :cl-user)

(defpackage :mlep
  (:use :cl)
  (:export
   :*iris* :*lenses* :*wages* :*heights-weights*
   :random-from-to
   :plot-values :plot-points
   :run
   :k-means :k :data-set :distance :means :classify
   :k-nearest-neighbors :set-labels :test-set
   :naive-bayes
   :perceptron
   :markov-chain :synthesize :order :analyze :probabilities :unique
   :max-likelihood
   :neuronal-network :forward :learning-rate
   :imputer :transform)
  (:documentation "@code{mlep} is a Machine Learning library for Educational Purposes.

It aims at providing a collection of simple machine learning algorithms with the
following claims:
@itemize{
 @item{to use only ANSI Common Lisp (thus to be implementation independent)}
 @item{to be fairly easy to use so that even intermediate Common Lisp programmers should
   be able to use this library instantly without pain}
 @item{to provide a tutorial-style documentation so that one should get to know this
   library easily}}"))