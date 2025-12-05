;;;; ./src/commands/push.lisp

(defpackage :cl-git-tree/commands/push
  (:use :cl)
  (:export cmd-push
           push-repo))

(in-package :cl-git-tree/commands/push)

(defun push-repo (repo-dir args)
  "Делает git push для всех remotes текущей ветки и печатает статус."
  (declare (ignore args))
  (let* ((branch (cl-git-tree/git-utils:current-branch repo-dir))
         (remotes (cl-git-tree/git-utils:repo-remotes repo-dir))
         (ws (cl-git-tree/loc:make-workspace repo-dir)))
    (dolist (remote remotes)
      (let ((provider (cl-git-tree/loc:find-location remote)))
        (if provider
            ;; If a provider (location) is registered for this remote, delegate to repo-push
            (handler-case
                (progn
                  (cl-git-tree/loc:repo-push ws provider :branch branch)
                  ;; repo-push implementations may print their own status
                  )
              (error (e)
                (format t "❌ [~A] Ошибка: ~A~%" remote e)))
            ;; Otherwise fall back to raw git push
            (multiple-value-bind (_out err code)
                (cl-git-tree/git-utils:git-run repo-dir "push" remote branch)
              (declare (ignore _out))
              (if (zerop code)
                  (format t "✅ ~A: push ~A/~A успешно~%" repo-dir remote branch)
                  (format t "❌ ~A: push ~A/~A завершился с кодом ~A:~%~A"
                          repo-dir remote branch code err))))))))

(defun cmd-push (&rest args)
  "CLI-команда: выполнить git push для всех remotes текущей ветки во всех репозиториях."
  (cond
    ((member "--help" args :test #'string=)
     (format t "Выполняет git push для всех remotes текущей ветки во всех git-репозиториях.~%~%")
     (format t "Использование:~%  git-tree push~%")
     (format t "Пример:~%  git-tree push~%"))
    (t
     (cl-git-tree/fs:with-repo #'push-repo args))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "push" #'cmd-push "Выполнить git push во всех репозиториях"))
