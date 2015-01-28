# cl-mlep

`cl-mlep` is a Common Lisp Machine Learning library for Educational Purposes.

It aims at providing a collection of simple machine learning algorithms with the following claims:

* to be implementation independent
* to be fairly easy to use so that even intermediate Common Lisp programmers should be able to use this library instantly without pain
* to provide a tutorial-style documentation so that one should get to know this library easily
* the main part of cl-mlep should run without any dependencies (except for ASDF for its system definition). However, some algorithms need dependencies: they are to be found in the separate package `mlep-add`.

In `/doc/index.html` you get an HTML documentation including usage examples (`/doc/usage_examples.html`) and an API (`/doc/api/index.html`).

To start straight away, you can load `load.lisp` or `load-with-add.lisp`. The latter one includes the prior one, but needs some dependencies for providing even more machine learning algorithms! You find more information about this in the usage examples, section *Why Additional?* (`/doc/usage_examples.html#about-add`).

`cl-mlep` was written by Frank Zalkow and is licensed under a [Creative Commons Attribution 4.0 International license](http://creativecommons.org/licenses/by/4.0/).