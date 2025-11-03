;;;; ./src/commands/pull.lisp

(defpackage :cl-git-tree/commands/pull
  (:use :cl)
  (:export cmd-pull))

(in-package :cl-git-tree/commands/pull)

(defun pull-repo (repo-dir args)
  "Делает git pull для всех remotes текущей ветки и печатает статус."
  (declare (ignore args))
  (let* ((branch (cl-git-tree/git-utils:current-branch repo-dir))
         (remotes (cl-git-tree/git-utils:repo-remotes repo-dir)))
    (dolist (remote remotes)
      (multiple-value-bind (out err code)
          (cl-git-tree/git-utils:git-run repo-dir "pull" remote branch)
        (declare (ignore out))
        (if (zerop code)
            (format t "✔ ~A: pull ~A/~A успешно~%"
                    repo-dir remote branch)
            (format t "❌ ~A: pull ~A/~A завершился с кодом ~A:~%~A"
                    repo-dir remote branch code err))))))

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
