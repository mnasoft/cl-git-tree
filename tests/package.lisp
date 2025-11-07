;;;; tests/package.lisp

(defpackage :cl-git-tree/tests
  (:use #:cl #:fiveam)
  (:export run-tests)
  (:documentation " :cl-git-tree/tests "))

(in-package :cl-git-tree/tests)

(defun run-tests () (run! 'all))
