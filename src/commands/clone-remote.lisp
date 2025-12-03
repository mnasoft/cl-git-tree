;;;; ./src/commands/clone-remote.lisp

(defpackage :cl-git-tree/commands/clone-remote
  (:use :cl)
  (:export cmd-clone-remote))

(in-package :cl-git-tree/commands/clone-remote)

(defun clone-repo-to-remote (repo-dir args)
  "Klonira repo na remote server."
  (let* ((loc-key (first args))
         (location (cl-git-tree/loc:find-location loc-key)))
    (unless location
      (format t "Warning: location ~A not found~%" loc-key)
      (return-from clone-repo-to-remote))
    (let* ((url (cl-git-tree/loc:<location>-url-git location))
           (parsed (cl-git-tree/shell-utils:parse-ssh-url url)))
      (unless parsed
        (format t "Warning: ~A invalid SSH URL: ~A~%" repo-dir url)
        (return-from clone-repo-to-remote))
      (destructuring-bind (user host path) parsed
        (let* ((repo-name (cl-git-tree/fs:repo-name repo-dir))
               (bare-name (format nil "~A.git" repo-name))
               (target-path (format nil "~A/~A" (string-right-trim "/" path) bare-name)))
          (let ((remotes (cl-git-tree/git-utils:repo-remotes repo-dir)))
            (unless (member loc-key remotes :test #'string=)
              (format t "Warning: ~A location ~A not remote~%" repo-name loc-key)
              (return-from clone-repo-to-remote)))
          (multiple-value-bind (out err code)
              (cl-git-tree/shell-utils:ssh-run user host (format nil "test -d ~A" target-path))
            (declare (ignore err out))
            (if (zerop code)
                (format t "Warning: ~A already exists on ~A:~A~%" repo-name host target-path)
                (handler-case
                    (progn
                      (format t "Preparing server ~A@~A~%" user host)
                      (multiple-value-bind (o e c)
                          (cl-git-tree/shell-utils:ssh-run user host 
                            (format nil "mkdir -p ~A" (string-right-trim "/" path)))
                        (declare (ignore o))
                        (when (not (zerop c))
                          (format t "Error: ~A: ~A~%" repo-name e)
                          (return-from clone-repo-to-remote)))
                      (format t "Cloning to ~A:~A~%" host target-path)
                      (multiple-value-bind (out err code)
                          (cl-git-tree/git-utils:git-run repo-dir "clone" "--bare" "."
                            (format nil "~A@~A:~A" 
                                    (or user (uiop:getenv "USER"))
                                    host
                                    target-path))
                        (declare (ignore out))
                        (if (zerop code)
                            (format t "OK: ~A -> ~A:~A~%" repo-name host target-path)
                            (format t "Error: ~A: clone failed: ~A~%" repo-name err))))
                  (error (e)
                    (format t "Error: ~A: ~A~%" repo-name e))))))))))

(defun cmd-clone-remote (&rest args)
  "Clone all repos to remote server."
  (cond
    ((or (null args) (string= (first args) "--help"))
     (format t "Clone all repos to remote server (via SSH).~%~%")
     (format t "Usage:~%  git-tree clone-remote LOC-KEY~%"))
    (t
     (let* ((location-name (first args))
            (loc (cl-git-tree/loc:find-location location-name)))
       (if (null loc)
           (format t "Warning: location ~A not found.~%" location-name)
           (cl-git-tree/fs:with-repo #'clone-repo-to-remote args))))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "clone-remote" #'cmd-clone-remote "Clone to remote server"))
