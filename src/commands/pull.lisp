;;;; ./src/commands/pull.lisp

(defpackage :cl-git-tree/commands/pull
  (:use :cl)
  (:export cmd-pull
           pull-repo))

(in-package :cl-git-tree/commands/pull)

(defun pull-repo (repo-dir args)
  "Делает git pull для всех remotes текущей ветки и печатает статус."
  (declare (ignore args))
  (let ((ws (cl-git-tree/loc:make-workspace repo-dir)))
    (cl-git-tree/loc:repo-pull-all ws)))

(defun cmd-pull (&rest args)
  "CLI-команда: найти все git-репозитории и выполнить pull для каждого."
  (cond
    ((member "--help" args :test #'string=)
     (format t "Выполняет git pull для всех remotes текущей ветки во всех git-репозиториях.~%~%")
     (format t "Использование:~%  git-tree pull~%")
     (format t "Пример:~%  git-tree pull~%"))
    (t
     (cl-git-tree/fs:with-repo #'pull-repo args))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "pull" #'cmd-pull "Выполнить git pull во всех репозиториях"))
