;;;; ./src/commands/push.lisp

(defpackage :cl-git-tree/commands/push
  (:use :cl)
  (:export cmd-push
           push-repo))

(in-package :cl-git-tree/commands/push)

(defun push-repo (repo-dir args)
  "Выполняет git push для всех веток на все remotes."
  (declare (ignore args))
  (let ((ws (cl-git-tree/loc:make-workspace repo-dir)))
    (cl-git-tree/loc:repo-push-all ws)))

(defun cmd-push (&rest args)
  "CLI-команда: выполнить git push для всех веток на все remotes во всех репозиториях."
  (cond
    ((member "--help" args :test #'string=)
     (format t "Выполняет git push для всех локальных веток~%")
     (format t "на все зарегистрированные remotes во всех git-репозиториях.~%~%")
     (format t "Использование:~%  git-tree push~%")
     (format t "Пример:~%  git-tree push~%"))
    (t
     (cl-git-tree/fs:with-repo #'push-repo args))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "push" #'cmd-push "Выполнить git push всех веток на все remotes"))
