;;;; ./src/commands/commit.lisp

(defpackage :cl-git-tree/commands/commit
  (:use :cl)
  (:export cmd-commit))

(in-package :cl-git-tree/commands/commit)

(defun commit-repo (repo-dir message)
  "Делает git commit -am MESSAGE в указанном репо."
  (multiple-value-bind (_out err code)
      (cl-git-tree/git-utils:git-run repo-dir "commit" "-am" message)
    (declare (ignore _out))
    (if (zerop code)
        (format t "✔ ~A: commit ~A~%" repo-dir message)
        (format t "❌ ~A: ошибка commit:~%~A" repo-dir err))))

(defun cmd-commit (&rest args)
  "CLI-команда: выполнить git commit -am MESSAGE во всех репозиториях.
Если MESSAGE отсутствует, используется текущая дата."
  (let ((message (if args
                     (format nil "~{~A~^ ~}" args)
                     (multiple-value-bind (sec min hour day mon year)
                         (decode-universal-time (get-universal-time))
                       (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                               year mon day hour min sec)))))
    (cl-git-tree/fs:with-each-repo-simple
      (lambda (repo-dir)
        (commit-repo repo-dir message)))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "commit" #'cmd-commit "Сделать git commit -am MESSAGE во всех репозиториях"))

