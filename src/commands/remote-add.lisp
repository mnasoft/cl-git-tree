;;;; ./src/commands/remote-add.lisp

(defpackage :cl-git-tree/commands/remote-add
  (:use :cl)
  (:export add-remote-to-repo
           cmd-remote-add))

(in-package :cl-git-tree/commands/remote-add)

(defun add-remote-to-repo (repo-dir loc-key base-url)
  "Добавляет remote LOC-KEY в один репозиторий."
  (let* ((repo-name (cl-git-tree/fs:repo-name repo-dir))
         (remote-url (format nil "~A/~A.git" base-url repo-name)))
    (multiple-value-bind (out _err _code)
        (cl-git-tree/git-utils:git-run repo-dir "remote")
      (declare (ignore _err _code))
      (if (search loc-key out :test #'string=)
          (format t "⚠️  В ~A remote ~A уже существует~%" repo-dir loc-key)
          (progn
            (format t "→ ~A: git remote add ~A ~A~%"
                    repo-dir loc-key remote-url)
            (cl-git-tree/git-utils:git-run repo-dir "remote" "add" loc-key remote-url))))))

(defun cmd-remote-add (loc-key &rest _args)
  "CLI-команда: найти все git-репозитории и добавить remote LOC-KEY."
  (cl-git-tree/fs:with-each-repo loc-key #'add-remote-to-repo))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "remote-add" #'cmd-remote-add "Добавить remote во все репозитории"))
