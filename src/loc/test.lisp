;;;; ./src/workspace/workspace.lisp

(in-package :cl-git-tree/loc)

(defparameter *ws* (make-workspace #P"~/quicklisp/local-projects/clisp/cl-git-tree"))

(defparameter *lc* (make-instance '<local>))
(defparameter *gh* (make-instance '<github>))




(repo-add *ws* :all t)

(repo-commit *ws* "date")
;;;
(repo-commit *ws* "`date`")

(repo-push *gh* *ws*)
(repo-pull *gh* *ws* :branch "master")
(repo-delete *gh* *ws*)
(repo-status *ws*)

(<location>-id *gh*)

"~/quicklisp/local-projects/clisp/cl-git-tree"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *ws* (make-workspace #P"."))
(truename ".")

(cl-git-tree:load-config)
cl-git-tree:*config-path*  ; => #P"D:/home/_namatv/PRG/msys64/home/namatv/.git-tree/locations.lisp"
*locations* 
(alexandria:hash-table-values *locations*)

(progn 
  (defparameter *ws* (make-workspace #P"~/quicklisp/local-projects/clisp/cl-git-tree/./"))
  *ws*)

(repo-name *ws*)
#P"D:/home/_namatv/PRG/msys64/home/namatv/./"

