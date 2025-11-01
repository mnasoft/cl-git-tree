;;;; ./src/commands/remote-remove.lisp

(defpackage :cl-git-tree/commands/remote-remove
  (:use :cl)
  (:export cmd-remote-remove
           remove-remote-from-repo))

(in-package :cl-git-tree/commands/remote-remove)

(defun remove-remote-from-repo (repo-dir loc-key base-url)
  "Удаляет remote LOC-KEY из одного репозитория."
  (declare (ignore base-url))
  (multiple-value-bind (out _err _code)
      (cl-git-tree/git-utils:git-run repo-dir "remote")
    (declare (ignore _err _code))
    (if (search loc-key out :test #'string=)
        (progn
          (format t "→ ~A: git remote remove ~A~%" repo-dir loc-key)
          (cl-git-tree/git-utils:git-run repo-dir "remote" "remove" loc-key))
        (format t "⚠️  В ~A remote ~A не найден~%" repo-dir loc-key))))

(defun cmd-remote-remove (loc-key &rest _args)
  "CLI-команда: найти все git-репозитории и удалить remote LOC-KEY."
  (cl-git-tree/fs:with-each-repo loc-key #'remove-remote-from-repo))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "remote-remove" #'cmd-remote-remove "Удалить remote во всех репозиториях"))
