;;;; ./src/commands/create-repo.lisp

(defpackage :cl-git-tree/commands/create-repo
  (:use :cl)
  (:export cmd-create-repo))

(in-package :cl-git-tree/commands/create-repo)

(defun create-repo-for-workspace (repo-dir args)
  "Helper called by with-repo for each repo-dir.
ARGS is the original command args (first element is location key).
This will build a workspace object and call `repo-create` for the
location found in the global table." 
  (let* ((loc-key (first args))
         (loc (cl-git-tree/loc:find-location loc-key)))
    (unless loc
      (format t "Warning: location ~A not found~%" loc-key)
      (return-from create-repo-for-workspace))
    (let* ((ws (cl-git-tree/loc:make-workspace repo-dir))
           (private (member "--private" args :test #'string=)))
      (if private
          (cl-git-tree/loc:repo-create ws loc :private t)
          (cl-git-tree/loc:repo-create ws loc)))))

(defun cmd-create-repo (&rest args)
  "Create repositories for all local repos according to provider.

Usage: git-tree create-repo LOC-KEY [--private]

LOC-KEY — ключ локации (как в `~/.git-tree/locations.configure`).
If `--private` is provided, the keyword :private is passed to `repo-create`.
"
  (cond
    ((or (null args) (string= (first args) "--help"))
     (format t "Create repositories according to provider.~%~%")
     (format t "Usage:~%  git-tree create-repo LOC-KEY [--private]~%"))
    (t
     (let ((loc-key (first args)))
       (if (null (cl-git-tree/loc:find-location loc-key))
           (format t "Warning: location ~A not found.~%" loc-key)
           (cl-git-tree/fs:with-repo #'create-repo-for-workspace args))))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "create-repo" #'cmd-create-repo "Create repos on provider (via repo-create)"))
