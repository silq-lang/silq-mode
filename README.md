silq-mode: Major mode for Silq in emacs
=======================================

This is a rudimentary emacs mode for Silq that provides syntax highlighting and indentation.
It sets the default compile command to invoke the current file using the Silq type checker.

Installation
------------
```console
$ git clone https://github.com:silq-lang/silq-mode ~/.emacs.d/silq-mode
```
Then add the following elisp code to your `~/.emacs`:
```elisp
;; silq
(add-to-list 'load-path "~/.emacs.d/silq-mode")
(require 'silq-mode)
```
To enhance performance, you can use `M-x byte-compile-file` and pass `~/.emacs.d/silq-mode/silq-mode.el`.
