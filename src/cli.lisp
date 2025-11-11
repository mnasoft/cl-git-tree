;;;; ./src/cli.lisp

(defpackage :cl-git-tree/cli
  (:use :cl)
  (:documentation
   "Подсистема CLI: точка входа и справка по доступным командам git-tree.")
  (:export main))

(in-package :cl-git-tree/cli)

(defun print-help ()
  "Выводит справку по доступным командам git-tree."
  (cl-git-tree/dispatch:show-version)
  (format t "Usage: git-tree <command> [args...]~%~%")
  (format t "Доступные команды:~%")
  (loop :for (cmd-name cmd-func cmd-desc) :in cl-git-tree/dispatch:*commands*
        :do
           (format t "  ~15A - ~A~%" cmd-name cmd-desc)))

(defun main (argv)
  "CLI‑точка входа. ARGV — список аргументов командной строки."
  (cl-git-tree:load-config)
  (let ((args (rest argv))) ; первый элемент — имя скрипта
    (cond
      ((or (null args)
           (string= (first args) "--help")
           (string= (first args) "help"))
       (print-help))
      ((string= (first args) "version")
       (cl-git-tree/dispatch:show-version))
      (t
       (cl-git-tree/dispatch:dispatch-command (first args) (rest args))))))
