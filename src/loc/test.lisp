;;;; ./src/workspace/workspace.lisp

(in-package :cl-git-tree/loc)

(defparameter *ws* (make-workspace #P"~/quicklisp/local-projects/clisp/cl-git-tree"))

(defparameter *lc* (make-instance '<local>))
(defparameter *gh* (make-instance '<github>))

(repo-create *gh* *ws*)

(repo-add *gh* *ws*)
(repo-push *gh* *ws*)
(repo-pull *gh* *ws* :branch "master")
(repo-delete *gh* *ws*)
(repo-status *ws*)

(<location>-id *gh*)

"~/quicklisp/local-projects/clisp/cl-git-tree"




