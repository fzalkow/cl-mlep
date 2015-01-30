;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage :mlep-asd
  (:use :cl :asdf))

(in-package :mlep-asd)

(defsystem :mlep
    :name "mlep"
    :version "0.0.1"
    :maintainer "Frank Zalkow <frank_zalkow@web.de>"
    :author "Frank Zalkow <frank_zalkow@web.de>"
    :licence "MIT License <http://opensource.org/licenses/MIT>"
    :description
    "A Common Lisp machine learning library for educational purposes."
    :long-description
    "mlep is a Machine Learning library for Educational Purposes.

It aims at providing a collection of simple machine learning algorithms with the
following claims:
 * to be implementation independent
 * to be fairly easy to use so that even intermediate Common Lisp programmers should
   be able to use this library instantly without pain
 * to provide a tutorial-style documentation so that one should get to know this
   library easily"
    :serial t
    :pathname #P"src/"
    :components ((:file "package")
                 (:module "macros"
			  :components ((:file "macros")))
		 (:module "datasets"
			  :components ((:file "iris")
                                       (:file "heights-weights")
                                       (:file "lenses")
                                       (:file "wages")))
                 (:module "utils"
			  :components ((:file "number-utils")
				       (:file "list-utils")
                                       (:file "array-utils")
                                       (:file "constants")
                                       (:file "functions")
                                       (:file "plot")))
		 (:module "core"
			  :components ((:file "generic")
                                       (:file "k-means")
                                       (:file "k-nearest")
                                       (:file "markov-chain")
                                       (:file "naive-bayes")
                                       (:file "perceptron")
                                       (:file "likelihood")
                                       (:file "neuronal")))))