;;;; ./src/commands/status.lisp

(defpackage :cl-git-tree/commands/status
  (:use :cl)
  (:export cmd-status
           status-repo))

(in-package :cl-git-tree/commands/all)

(defun status-repo (repo-dir args)
  "Выводит git status для одного репозитория REPO-DIR."
  (multiple-value-bind (out err code)
      (cl-git-tree/git-utils:git-run repo-dir "status" "--short")
    (if (zerop code)
        (progn
          (format t "~%📁 ~A~%" repo-dir)
          (if (string= out "")
              (format t "✔ Чисто~%")
              (format t "~A~%" out)))
        (format t "❌ ~A: git status завершился с кодом ~A:~%~A~%" repo-dir code err))))


(defun cmd-status (&rest args)
  "CLI-команда: рекурсивно вызвать git status во всех git-репозиториях."
  (cond
    ((member "--help" args :test #'string=)
     (format t "Показывает git status во всех git-репозиториях, найденных в дереве.~%~%")
     (format t "Использование:~%  git-tree status~%")
     (format t "Пример:~%  git-tree status~%"))
    (t
     (cl-git-tree/fs:with-repo #'status-repo args))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "status" #'cmd-status "Показать git status во всех репозиториях"))
