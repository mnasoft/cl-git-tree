;;;; ./src/commands/remote-remove.lisp

(defpackage :cl-git-tree/commands/remote-remove
  (:use :cl)
  (:import-from cl-git-tree
                *locations*
                location-url-git)
  (:import-from cl-git-tree/fs
                with-each-repo)
  (:import-from cl-git-tree/git-utils
                git-run
                current-branch
                repo-remotes)
  (:export :run))

(in-package :cl-git-tree/commands/remote-remove)

(defun remove-remote-from-repo (repo-dir loc-key base-url)
  "Удаляет remote <loc-key> из одного репозитория."
  (declare (ignore base-url))
  (multiple-value-bind (out _err _code)
      (git-run repo-dir "remote")
    (declare (ignore _err _code))
    (if (search loc-key out :test #'string=)
        (progn
          (format t "→ ~A: git remote remove ~A~%" repo-dir loc-key)
          (git-run repo-dir "remote" "remove" loc-key))
        (format t "⚠️  В ~A remote ~A не найден~%" repo-dir loc-key))))

(defun run (loc-key &rest _args)
  "Находит все git-репозитории и вызывает remove-remote-from-repo для каждого."
  (with-each-repo loc-key #'remove-remote-from-repo))

(push (cons "remote-remove" #'run) cl-git-tree:*commands*)
