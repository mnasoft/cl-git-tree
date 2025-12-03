;;;; ./src/commands/unclone-remote.lisp

(defpackage :cl-git-tree/commands/unclone-remote
  (:use :cl)
  (:export cmd-unclone-remote))

(in-package :cl-git-tree/commands/unclone-remote)

(defun unclone-repo-from-remote (repo-dir args)
  "Remove bare repo from remote server."
  (let* ((loc-key (first args))
         (location (cl-git-tree/loc:find-location loc-key)))
    (unless location
      (format t "Warning: location ~A not found~%" loc-key)
      (return-from unclone-repo-from-remote))
    (let* ((url (cl-git-tree/loc:<location>-url-git location))
           (parsed (cl-git-tree/shell-utils:parse-ssh-url url)))
      (unless parsed
        (format t "Warning: ~A invalid SSH URL: ~A~%" repo-dir url)
        (return-from unclone-repo-from-remote))
      (destructuring-bind (user host path) parsed
        (let* ((repo-name (cl-git-tree/fs:repo-name repo-dir))
               (bare-name (format nil "~A.git" repo-name))
               (target-path (format nil "~A/~A" (string-right-trim "/" path) bare-name)))
          (let ((remotes (cl-git-tree/git-utils:repo-remotes repo-dir)))
            (unless (member loc-key remotes :test #'string=)
              (format t "Warning: ~A location ~A not remote~%" repo-name loc-key)
              (return-from unclone-repo-from-remote)))
          (multiple-value-bind (out err code)
              (cl-git-tree/shell-utils:ssh-run user host (format nil "test -d ~A" target-path))
            (declare (ignore err out))
            (if (zerop code)
                (handler-case
                    (progn
                      (format t "Deleting ~A:~A~%" host target-path)
                      (multiple-value-bind (o e c)
                          (cl-git-tree/shell-utils:ssh-run user host 
                            (format nil "rm -rf ~A" target-path))
                        (declare (ignore o))
                        (if (zerop c)
                            (format t "OK: ~A deleted from ~A~%" repo-name host)
                            (format t "Error: ~A: ~A~%" repo-name e))))
                  (error (e)
                    (format t "SSH error: ~A: ~A~%" repo-name e)))
                (format t "Warning: ~A not found on ~A:~A~%" repo-name host target-path))))))))

(defun cmd-unclone-remote (&rest args)
  "Remove all repos from remote server."
  (cond
    ((or (null args) (string= (first args) "--help"))
     (format t "Delete all bare repos from remote server (via SSH).~%~%")
     (format t "Usage:~%  git-tree unclone-remote LOC-KEY~%"))
    (t
     (let* ((location-name (first args))
            (loc (cl-git-tree/loc:find-location location-name)))
       (if (null loc)
           (format t "Warning: location ~A not found.~%" location-name)
           (cl-git-tree/fs:with-repo #'unclone-repo-from-remote args))))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "unclone-remote" #'cmd-unclone-remote "Delete from remote server"))
