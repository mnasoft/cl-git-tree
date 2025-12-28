;;;; ./src/workspace/workspace.lisp

(in-package :cl-git-tree/loc)

(cl-git-tree:load-config)

;;;;;;;;;;;;;;;;;;;;

(defparameter *ws* (make-workspace #P"~/quicklisp/local-projects/clisp/cl-git-tree"))

(defparameter *lc* (make-instance '<local>))
(defparameter *gh* (make-instance '<github>))

001
