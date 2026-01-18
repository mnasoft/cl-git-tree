;;;; ./src/commands/remote-add.lisp

(defpackage :cl-git-tree/commands/remote-add
  (:use :cl)
  (:export add-remote-to-repo
           cmd-remote-add))

(in-package :cl-git-tree/commands/remote-add)

(defun add-remote-to-repo (repo-dir args) 
  "Добавляет remote LOC-KEY в один репозиторий."
  (let* ((loc-key (first args))
         (pr (cl-git-tree/loc:find-location loc-key))
         (ws (cl-git-tree/loc:make-workspace repo-dir)))
    (cl-git-tree/loc:remote-add ws pr)))

(defun cmd-remote-add (&rest args)
  "CLI-команда: добавить remote LOC-KEY во все git-репозитории.

Пример использования:
  git-tree remote-add gh"
  (cond
    ;; Показать справку
    ((or (null args)
         (member "--help" args :test #'string=))
     (format t "Добавляет remote во все git-репозитории.~%~%")
     (format t "Использование:~%  git-tree remote-add LOC-KEY~%")
     (format t "Пример:~%  git-tree remote-add gh~%"))
    ;; Запуск по дереву
    (t
     (cl-git-tree/fs:with-repo #'add-remote-to-repo args))))
