;;;; ./src/commands/push.lisp

(defpackage :cl-git-tree/commands/push
  (:use :cl)
  (:export cmd-push))

(in-package :cl-git-tree/commands/push)

(defun push-repo (repo-dir)
  "Делает git push для всех remotes текущей ветки и печатает статус."
  (let ((branch (cl-git-tree/git-utils:current-branch repo-dir))
        (remotes (cl-git-tree/git-utils:repo-remotes repo-dir)))
    (dolist (remote remotes)
      (multiple-value-bind (_out err code)
          (cl-git-tree/git-utils:git-run repo-dir "push" remote branch)
        (declare (ignore _out))
        (if (zerop code)
            (format t "✔ ~A: push ~A/~A успешно~%" repo-dir remote branch)
            (format t "❌ ~A: push ~A/~A завершился с кодом ~A:~%~A"
                    repo-dir remote branch code err))))))

(defun cmd-push (&rest _args)
  "CLI-команда: найти все git-репозитории и выполнить push для каждого."
  (declare (ignore _args))
  (cl-git-tree/fs:with-each-repo-simple #'push-repo))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "push" #'cmd-push "Выполнить git push во всех репозиториях"))
