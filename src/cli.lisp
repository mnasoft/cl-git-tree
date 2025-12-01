;;;; ./src/cli.lisp

(defpackage :cl-git-tree/cli
  (:use :cl)
  (:documentation
   "Подсистема CLI: точка входа и справка по доступным командам git-tree.")
  (:export main
           print-help-with-config))

(in-package :cl-git-tree/cli)

(defun print-help ()
  "Выводит справку по доступным командам git-tree."
  (cl-git-tree/dispatch:show-version)
  (format t "Usage: git-tree <command> [args...]~%~%")
  (format t "Доступные команды:~%")
  (loop :for (cmd-name cmd-func cmd-desc) :in cl-git-tree/dispatch:*commands*
        :do
           (format t "  ~15A - ~A~%" cmd-name cmd-desc)))

(defun print-help-with-config ()
  "Print help and list configuration files used by git-tree."
  (print-help)
  (format t "~%Configuration files:~%")
  (format t "  Locations config: ~S~%" cl-git-tree:*config-path*)
  (format t "  File patterns:    ~S~%" cl-git-tree/config:*patterns-path*))


(defun main (argv)
  "CLI‑точка входа. ARGV — список аргументов командной строки."
  (cl-git-tree:load-config)
  (let ((args (rest argv))) ; первый элемент — имя скрипта
    (cond
      ;; No args, explicit --help or `help` → show help
      ((or (null args)
           (string= (first args) "--help")
           (string= (first args) "help"))
       (print-help-with-config))
      ;; `git tree --` (separator). If no additional args or next is --help, show help
      ((and (string= (first args) "--")
            (or (null (rest args))
                (string= (second args) "--help")
                (string= (second args) "help")))
       (print-help-with-config))
      ((string= (first args) "version")
       (cl-git-tree/dispatch:show-version))
      (t
       (cl-git-tree/dispatch:dispatch-command (first args) (rest args))))))
