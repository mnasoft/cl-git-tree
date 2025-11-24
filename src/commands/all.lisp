;;;; ./src/commands/all.lisp

(defpackage :cl-git-tree/commands/all
  (:use :cl)
  (:export cmd-all
           all-in-one-repo))

(in-package :cl-git-tree/commands/all)

(defun all-in-one-repo (repo-dir args)
  "Выполняет pull → add → commit → push в одном репозитории."
  (format t "~%📁 ~A~%" repo-dir)
  (cl-git-tree/commands/pull:pull-repo     repo-dir args)
  (cl-git-tree/commands/add:add-repo       repo-dir args)
  (cl-git-tree/commands/commit:commit-repo repo-dir args)
  (cl-git-tree/commands/push:push-repo     repo-dir args))

(defun cmd-all (&rest args)
  "CLI-команда: выполнить pull → add → commit → push для каждого репозитория.
Если MESSAGE для commit отсутствует, используется текущая дата."
  (cond
    ((member "--help" args :test #'string=)
     (format t "Выполняет pull → add → commit → push во всех git-репозиториях.~%~%")
     (format t "Использование:~%  git-tree all [MESSAGE...]~%")
     (format t "Если MESSAGE не указан, используется текущая дата.~%")
     (format t "Пример:~%  git-tree all \"Обновление зависимостей\"~%"))
    (t
     (cl-git-tree/fs:with-repo #'all-in-one-repo args))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "all" #'cmd-all "Выполнить pull → add → commit → push"))
