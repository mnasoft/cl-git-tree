;;;; ./src/commands/remote-readd.lisp

(defpackage :cl-git-tree/commands/remote-readd
  (:use :cl)
  (:export cmd-remote-readd))

(in-package :cl-git-tree/commands/remote-readd)

(defun readd-remote-to-repo (repo-dir args)
  "Удаляет и заново добавляет remote LOC-KEY в один репозиторий."
  (let* ((loc-key (first args))
         (provider (cl-git-tree/loc:find-location loc-key)))
    (if provider
        (let ((ws (cl-git-tree/loc:make-workspace repo-dir)))
          (cl-git-tree/loc:remote-readd ws provider))
        (format t "⚠️  Локация '~A' не найдена в *locations*~%" loc-key))))

(defun cmd-remote-readd (&rest args)
  "CLI-команда: найти все git-репозитории и заново добавить remote LOC-KEY."
  (cl-git-tree/fs:with-repo #'readd-remote-to-repo args))

;; Регистрация перемещена в remote.lisp (используется как подкоманда 'remote readd')
