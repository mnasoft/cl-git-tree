;;;; ./src/shell-utils/package.lisp

(defpackage :cl-git-tree/shell-utils
  (:use :cl :uiop)
  (:export split-args-by-keys
           shell-run
           parse-ssh-url
           ssh-run
           ssh-cmd))

(in-package :cl-git-tree/shell-utils)
