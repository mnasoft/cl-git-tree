(in-package :cl-git-tree/loc)

(progn 
  (cl-git-tree:load-config)

  (defparameter *ws*
    (make-workspace
     "~/quicklisp/local-projects/clisp/cl-git-tree"))

  (defparameter *lc*
    (find-location "lc")))

(let ((ws *ws*)
      (provider *lc*))
  (remote-import-connect ws provider)
  ;(remote-import-disconnect ws provider)
  )




(let* ((root (git-root ws))
       (repo (repo-name ws))
       (loc-id (<location>-id provider))
       (remote (or remote-name (format nil "~A-import" loc-id)))
       (xz-base (<location>-url-xz provider))
       (git-base (<location>-url-git provider))
       (candidates '())
       (found nil)
       )
  (when xz-base
      
    (push (uiop:ensure-directory-pathname
           (merge-pathnames (format nil "~A.git/" repo)
                            (uiop:ensure-directory-pathname xz-base)))
          candidates))
  candidates)
