;;;; ./src/git-utils/package.lisp

(defpackage :cl-git-tree/git-utils
  (:use :cl)
  (:export
   git-run
   normalize-path-for-git
   current-branch
   repo-remotes)
  (:documentation
   "Вспомогательные функции для работы с git: запуск команд, определение текущей ветки, список remotes."))

(in-package :cl-git-tree/git-utils)
