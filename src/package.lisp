(defpackage :cl-git-tree
  (:use :cl)
  (:export *locations*)
  (:export *commands*)
  (:export run
           load-config
           dispatch-command))

(in-package :cl-git-tree)
