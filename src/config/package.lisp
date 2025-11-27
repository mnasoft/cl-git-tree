;;;; ./src/config/package.lisp

(defpackage :cl-git-tree/config
  (:use :cl)
  (:export *tracked-patterns*
           *excludes-patterns*))

(in-package :cl-git-tree/config)
