;;;; ./src/commands/push.lisp

(defpackage :cl-git-tree/commands/push
  (:use :cl)
  (:import-from cl-git-tree
                *locations*
                location-url-git
                )
  (:import-from cl-git-tree/fs
                repo-name
                with-each-repo
                with-each-repo-simple
                )
  (:import-from cl-git-tree/git-utils
                git-run
                current-branch
                repo-remotes
                )
  (:export run))

(in-package :cl-git-tree/commands/push)

(defun push-repo (repo-dir)
  "Делает git push для всех remotes текущей ветки и печатает статус."
  (let ((branch (current-branch repo-dir))
        (remotes (repo-remotes repo-dir)))
    (dolist (remote remotes)
      (multiple-value-bind (_out err code)
          (git-run repo-dir "push" remote branch)
        (declare (ignore _out))
        (if (zerop code)
            (format t "✔ ~A: push ~A/~A успешно~%" repo-dir remote branch)
            (format t "❌ ~A: push ~A/~A завершился с кодом ~A:~%~A"
                    repo-dir remote branch code err))))))

(defun run (&rest _args)
  "Находит все git-репозитории и вызывает push-repo для каждого."
  (with-each-repo-simple #'push-repo))

(push (cons "push" #'run) cl-git-tree:*commands*)
