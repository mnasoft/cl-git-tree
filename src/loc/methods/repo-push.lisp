(in-package :cl-git-tree/loc)

(defmethod repo-push ((provider <provider>) (ws <workspace>)
                      &key (remote (<location>-id provider)) branch force tags set-upstream
                      &allow-other-keys)
  "Выполнить git push на GitHub."
  (let ((root (git-root ws))
        (args '()))
    (when branch       (push branch           args))
    (when force        (push "--force"        args))
    (when tags         (push "--tags"         args))
    (when set-upstream (push "--set-upstream" args))
    (push remote  args)
    (push "push"  args)
    (push "git"   args)
    (multiple-value-bind (stdout stderr code)
        (apply #'cl-git-tree/shell-utils:shell-run root args)
      (cond
        ((zerop code)
         (format t "🚀 Репозиторий ~A успешно отправлен на ~A~%"
                 (repo-name ws) remote))
        (t
         (format t "❌ Ошибка при push на ~A: ~A ~A~%" remote stdout stderr))))
    ws))
