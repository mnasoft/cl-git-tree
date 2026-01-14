;;;; ./src/package.lisp

(defpackage :cl-git-tree
  (:use :cl)
  (:export 
           load-config
           reset-config
           *config-path*))

(in-package :cl-git-tree)

