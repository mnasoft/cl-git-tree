(in-package :cl-git-tree)

(defun show-help ()
  (format t "Доступные команды: readd, remake-xz, info, help, version~%"))

(defun show-version ()
  (let* ((sys (asdf:find-system :cl-git-tree))
         (ver (asdf:component-version sys)))
    (format t "cl-git-tree version ~A~%" ver)))

(push (cons "version" #'show-version) *commands*)

(defun dispatch-command (cmd args)
  (let ((entry (assoc cmd *commands* :test #'string=)))
    (if entry
        (apply (cdr entry) args)
        (format t "Неизвестная команда: ~A~%" cmd))))
