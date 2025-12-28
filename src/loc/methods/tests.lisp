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

(let ((ws *ws*)
      (provider *lc*))
  (remote-import-cleanup-dir ws provider :remote-name "lc-import" ))

(let ((ws *ws*)
      (provider *lc*))
  (repo-transport-import ws provider))


((path-str (namestring (pathname target-path))))

(namestring (pathname #P"~/.git-tree/xz/lc/cl-git-tree.git/"))

(cl-git-tree/shell-utils:shell-run-single "." "rm" "-rf"
                                          (namestring
                                          (probe-file #P"~/.git-tree/xz/lc/cl-git-tree.git/")))


(cl-git-tree/loc:expand-path *ws* "~/.git-tree")


