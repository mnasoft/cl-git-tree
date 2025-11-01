;;;; ./src/commands/info.lisp

(defpackage :cl-git-tree/commands/info
  (:use :cl)
  (:export cmd-info))

(in-package :cl-git-tree/commands/info)

(defun cmd-info (&rest _args)
  "CLI-команда: вывести список всех доступных локаций."
  (declare (ignore _args))
  (format t "~%=== Список локаций ===~%")
  (maphash (lambda (key loc)
             (format t "~A: ~A~%   Git: ~A~%   TAR: ~A~%   XZ: ~A~%~%"
                     key
                     (cl-git-tree/loc:<location>-name loc)
                     (cl-git-tree/loc:<location>-url-git loc)
                     (cl-git-tree/loc:<location>-tar loc)
                     (cl-git-tree/loc:<location>-url-xz loc)))
           cl-git-tree/loc:*locations*))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "info" #'cmd-info "Показать список всех доступных локаций"))
