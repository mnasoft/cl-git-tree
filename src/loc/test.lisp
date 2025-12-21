;;;; ./src/workspace/workspace.lisp

(in-package :cl-git-tree/loc)

(cl-git-tree:load-config)

;;;;;;;;;;;;;;;;;;;;

(defparameter *ws* (make-workspace #P"~/quicklisp/local-projects/clisp/cl-git-tree"))

(defparameter *lc* (make-instance '<local>))
(defparameter *gh* (make-instance '<github>))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *ws* (make-workspace #P"."))
(truename ".")


cl-git-tree:*config-path*  ; => #P"D:/home/_namatv/PRG/msys64/home/namatv/.git-tree/locations.lisp"
*locations* 
(alexandria:hash-table-values *locations*)

(progn 
  (defparameter *ws* (make-workspace #P"~/quicklisp/local-projects/clisp/cl-git-tree/./"))
  *ws*)

(repo-name *ws*)
#P"D:/home/_namatv/PRG/msys64/home/namatv/./"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-git-tree:load-config)
(defparameter *ws* (make-workspace #P"~/quicklisp/local-projects/cl-git-tree"))
(defparameter *lc* (find-location "lc"))

(uiop:delete-directory-tree
 (uiop:ensure-directory-pathname
  (remote-url *ws* *lc*))
 :validate t
 )

(cl-git-tree/shell-utils:shell-run-single "."
					  "rm" "-r"
					  (remote-url *ws* *lc*))

(cl-git-tree/shell-utils:shell-run-single "."
					  "mkdir"
					  (remote-url *ws* *lc*))

(namestring (uiop:ensure-directory-pathname (uiop:getenv "HOME")))

(expand-tilde-directory-path
 (cl-git-tree/loc:<location>-url-git *lc*))

(remote-delete *ws* *lc*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *ws* (make-workspace "~/quicklisp/local-projects/clisp/cl-git-tree"))

(loop :for pr-key :in (repo-provider-keys *ws*)
      :when (gethash pr-key *locations*)
      :collect :it)




(repo-provider-keys *ws*)
