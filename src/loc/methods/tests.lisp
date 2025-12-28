(in-package :cl-git-tree/loc)

(progn 
  (cl-git-tree:load-config)

  (defparameter *ws*
    (make-workspace
     "~/quicklisp/local-projects/clisp/cl-git-tree"))

  (defparameter *lc*
    (find-location "lc")))


remote-import-disconnect
remote-import-connect

(let ((ws *ws*)
      (provider *lc*))
  ;(remote-import-connect ws provider)
  (remote-import-disconnect ws provider) )
