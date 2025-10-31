;;;; ./src/commands/all.lisp

(defpackage :cl-git-tree/commands/all
  (:use :cl)
  (:export :run))

(in-package :cl-git-tree/commands/all)

(defun run (&rest args)
  "Команда all: pull → add → commit → push.
   Если MESSAGE для commit отсутствует, используется текущая дата."
  ;; pull
  (cl-git-tree/commands/pull:run)
  ;; add
  (cl-git-tree/commands/add:run)
  ;; commit (аргументы идут как сообщение)
  (apply #'cl-git-tree/commands/commit:run args)
  ;; push
  (cl-git-tree/commands/push:run))

(push (cons "all" #'run) cl-git-tree:*commands*)

