;;;; ./src/commands/remote-add.lisp

(defpackage :cl-git-tree/commands/remote-add
  (:use :cl)
  (:import-from cl-git-tree
                *locations*
                location-url-git
                )
  (:import-from cl-git-tree/fs
                repo-name
                with-each-repo)
  (:import-from cl-git-tree/git-utils
                git-run
                current-branch
                repo-remotes)
  (:export :run))

(in-package :cl-git-tree/commands/remote-add)

(defun add-remote-to-repo (repo-dir loc-key base-url)
  "Добавляет remote <loc-key> в один репозиторий."
  (let* ((repo-name (repo-name repo-dir))
         (remote-url (format nil "~A/~A.git" base-url repo-name)))
    (multiple-value-bind (out _err _code)
        (git-run repo-dir "remote")
      (declare (ignore _err _code))
      (if (search loc-key out :test #'string=)
          (format t "⚠️  В ~A remote ~A уже существует~%" repo-dir loc-key)
          (progn
            (format t "→ ~A: git remote add ~A ~A~%"
                    repo-dir loc-key remote-url)
            (git-run repo-dir "remote" "add" loc-key remote-url))))))


(defun run (loc-key &rest _args)
  "Находит все git-репозитории и вызывает add-remote-to-repo для каждого."
  (with-each-repo loc-key #'add-remote-to-repo))

(push (cons "remote-add" #'run) cl-git-tree:*commands*)
