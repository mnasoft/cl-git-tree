;;;; ./src/commands/remote-readd.lisp

(defpackage :cl-git-tree/commands/remote-readd
  (:use :cl)
  (:import-from cl-git-tree
                *locations*
                location-url-git)
  (:import-from cl-git-tree/fs
                repo-name
                with-each-repo)
  (:import-from cl-git-tree/commands/remote-add
                add-remote-to-repo)
  (:import-from cl-git-tree/commands/remote-remove
                remove-remote-from-repo)
  (:export :run))

(in-package :cl-git-tree/commands/remote-readd)

(defun readd-remote-to-repo (repo-dir loc-key base-url)
  "Удаляет и заново добавляет remote <loc-key> в один репозиторий."
  (remove-remote-from-repo repo-dir loc-key base-url)
  (add-remote-to-repo repo-dir loc-key base-url))

(defun run (loc-key &rest _args)
  "Находит все git-репозитории и вызывает readd-remote-to-repo для каждого."
  (with-each-repo loc-key #'readd-remote-to-repo))

(push (cons "remote-readd" #'run) cl-git-tree:*commands*)
