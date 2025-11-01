;;;; ./src/commands/remote-readd.lisp

(defpackage :cl-git-tree/commands/remote-readd
  (:use :cl)
  (:export cmd-remote-readd))

(in-package :cl-git-tree/commands/remote-readd)

(defun readd-remote-to-repo (repo-dir args)
  "Удаляет и заново добавляет remote LOC-KEY в один репозиторий."
  (cl-git-tree/commands/remote-remove:remove-remote-from-repo repo-dir args)
  (cl-git-tree/commands/remote-add:add-remote-to-repo         repo-dir args))

(defun cmd-remote-readd (&rest args)
  "CLI-команда: найти все git-репозитории и заново добавить remote LOC-KEY."
  (cl-git-tree/fs:with-repo #'readd-remote-to-repo args))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "remote-readd" #'cmd-remote-readd "Удалить и заново добавить remote во все репозитории"))
