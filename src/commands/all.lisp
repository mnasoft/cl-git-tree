;;;; ./src/commands/all.lisp

(defpackage :cl-git-tree/commands/all
  (:use :cl)
  (:export cmd-all))

(in-package :cl-git-tree/commands/all)

(defun cmd-all (&rest args)
  "CLI-команда: выполнить последовательность pull → add → commit → push.
Если MESSAGE для commit отсутствует, используется текущая дата."
  ;; pull
  (cl-git-tree/commands/pull:cmd-pull)
  ;; add
  (cl-git-tree/commands/add:cmd-add)
  ;; commit (аргументы идут как сообщение)
  (apply #'cl-git-tree/commands/commit:cmd-commit args)
  ;; push
  (cl-git-tree/commands/push:cmd-push))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "all" #'cmd-all "Выполнить pull → add → commit → push"))
