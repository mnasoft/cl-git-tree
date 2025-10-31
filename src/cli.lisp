;;;; ./src/cli.lisp
(defpackage :cl-git-tree/cli
  (:use :cl)
  (:import-from :cl-git-tree
                :load-config
                :dispatch-command
                :show-version)
  (:export :main))

(in-package :cl-git-tree/cli)

(defun print-help ()
  (show-version)
  (format t "Usage: git-tree <command> [args...]~%~%")
  (format t "Доступные команды:~%")
  (format t "  readd <loc> <repo> [remote]   - добавить git remote~%")
  (format t "  remake-xz <loc>               - пересоздать архив .xz~%")
  (format t "  info                          - показать список локаций~%")
  (format t "  version                       - показать версию~%")
  (format t "  help                          - эта справка~%"))

(defun main (argv)
  "Точка входа для CLI. argv — список аргументов командной строки."
  (load-config)
  (let ((args (rest argv))) ; первый элемент — имя скрипта
    (cond
      ((or (null args)
           (string= (first args) "--help")
           (string= (first args) "help"))
       (print-help))
      ((string= (first args) "version")
       (show-version))
      (t
       (dispatch-command (first args) (rest args))))))
