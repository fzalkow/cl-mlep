# cl-mlep

[![Quicklisp](http://quickdocs.org/badge/cl-mlep.svg)](http://quickdocs.org/cl-mlep)

`cl-mlep` is a Common Lisp Machine Learning library for Educational Purposes.

It aims at providing a collection of simple machine learning algorithms with the following claims:

* to be implementation independent
* to be fairly easy to use so that even intermediate Common Lisp programmers should be able to use this library instantly without pain
* to provide a tutorial-style documentation so that one should get to know this library easily
* the main part of cl-mlep should run without any dependencies (except for ASDF for its system definition). However, some algorithms need dependencies: they are to be found in the separate package `mlep-add`.

There is an HTML documentation ([http://fzalkow.github.io/cl-mlep](http://fzalkow.github.io/cl-mlep)) including [usage examples](http://fzalkow.github.io/cl-mlep/usage_examples.html) and an [API](http://fzalkow.github.io/cl-mlep/api/index.html). Its source is to be found in the branch `gh-pages`.

To start straight away, you can call `(ql:quickload :mlep)` or `(ql:quickload :mlep-add)` (or if you havn't [Quicklisp](http://www.quicklisp.org) installed load `load/load.lisp` or `load/load-with-add.lisp`). The latter one includes the prior one, but needs some dependencies for providing even more machine learning algorithms! You find more information about this in the usage examples, section [Why Additional?](http://fzalkow.github.io/cl-mlep/usage_examples.html#about-add).

If someone is interested in collaborating, please tell me. I'd be happy about this!