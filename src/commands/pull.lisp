;;;; ./src/commands/pull.lisp

(defpackage :cl-git-tree/commands/pull
  (:use :cl)
  (:import-from cl-git-tree
                *locations*
                location-url-git
                )
  (:import-from cl-git-tree/fs
                repo-name
                with-each-repo
                with-each-repo-simple)
  (:import-from cl-git-tree/git-utils
                git-run
                current-branch
                repo-remotes)
  (:export :run))

(in-package :cl-git-tree/commands/pull)

(defun pull-repo (repo-dir)
  "Делает git pull для всех remotes текущей ветки и печатает статус."
  (let* ((branch (current-branch repo-dir))
         (remotes (repo-remotes repo-dir)))
    (dolist (remote remotes)
      (multiple-value-bind (out err code)
        (git-run repo-dir "pull" remote branch)
        (declare (ignore out))
        (if (zerop code)
            (format t "✔ ~A: pull ~A/~A успешно~%"
                    repo-dir remote branch)
            (format t "❌ ~A: pull ~A/~A завершился с кодом ~A:~%~A"
                    repo-dir remote branch code err))))))

(defun run (&rest _args)
  "Находит все git-репозитории и вызывает pull-repo для каждого."
  (declare (ignore _args))
  (with-each-repo-simple #'pull-repo))

(push (cons "pull" #'run) cl-git-tree:*commands*)

