# cl-mlep

`cl-mlep` is a Common Lisp Machine Learning library for Educational Purposes.

It aims at providing a collection of simple machine learning algorithms with the following claims:

* to be implementation independent
* to be fairly easy to use so that even intermediate Common Lisp programmers should be able to use this library instantly without pain
* to provide a tutorial-style documentation so that one should get to know this library easily
* the main part of cl-mlep should run without any dependencies (except for ASDF for its system definition). However, some algorithms need dependencies: they are to be found in the separate package `mlep-add`.

There is an HTML documentation including [usage examples](http://fzalkow.github.io/cl-mlep/usage_examples.html) and an [API](http://fzalkow.github.io/cl-mlep/api/index.html) to be found in the branch `gh-pages` and to be viewed on [http://fzalkow.github.io/cl-mlep](http://fzalkow.github.io/cl-mlep).

To start straight away, you can load `load.lisp` or `load-with-add.lisp`. The latter one includes the prior one, but needs some dependencies for providing even more machine learning algorithms! You find more information about this in the usage examples, section [Why Additional?](http://fzalkow.github.io/cl-mlep/usage_examples.html#about-add).