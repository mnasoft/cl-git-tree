;;;; ./src/commands/remote-readd.lisp

(defpackage :cl-git-tree/commands/remote-readd
  (:use :cl)
  (:export cmd-remote-readd))

(in-package :cl-git-tree/commands/remote-readd)

(defun readd-remote-to-repo (repo-dir loc-key base-url)
  "Удаляет и заново добавляет remote LOC-KEY в один репозиторий."
  (cl-git-tree/commands/remote-remove:remove-remote-from-repo repo-dir loc-key base-url)
  (cl-git-tree/commands/remote-add:add-remote-to-repo repo-dir loc-key base-url))

(defun cmd-remote-readd (loc-key &rest _args)
  "CLI-команда: найти все git-репозитории и заново добавить remote LOC-KEY."
  (declare (ignore _args))
  (cl-git-tree/fs:with-each-repo loc-key #'readd-remote-to-repo))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "remote-readd" #'cmd-remote-readd "Удалить и заново добавить remote во все репозитории"))
