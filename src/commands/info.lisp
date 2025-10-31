(defpackage :cl-git-tree/commands/info
  (:use :cl)
  (:import-from
   :cl-git-tree
   :*locations*
   :location-name
   :location-url-git
   :location-url-xz
                :location-tar)
  (:export :run))

(in-package :cl-git-tree/commands/info)

(defun run (&rest _args)
  "Выводит список всех доступных локаций."
  (format t "~%=== Список локаций ===~%")
  (maphash (lambda (key loc)
             (format t "~A: ~A~%   Git: ~A~%   TAR: ~A~%   XZ: ~A~%~%"
                     key
                     (location-name loc)
                     (location-url-git loc)
                     (location-tar loc)
                     (location-url-xz loc)))
           *locations*))

(push (cons "info" #'run) cl-git-tree:*commands*)

