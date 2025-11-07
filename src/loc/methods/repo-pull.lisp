(in-package :cl-git-tree/loc)

(defmethod repo-pull ((provider <provider>) (ws <workspace>)
                      &key (remote (<location>-id provider)) branch rebase ff-only &allow-other-keys)
  "Выполнить git pull из указанного remote."
  (let* ((root (git-root ws))
         (args '()))
    (when branch   (push  branch args))
    (push remote args)
    (when ff-only  (push  "--ff-only" args))
    (when rebase   (push  "--rebase" args))
    (push "pull" args)
    (push "git"  args)
    ;; запуск
    (multiple-value-bind (stdout stderr code)
        (apply #'cl-git-tree/shell-utils:shell-run root args)
      (declare (ignore stderr))
      (cond
        ((zerop code)
         (format t "📥 Репозиторий ~A успешно обновлён из ~A~%"
                 (repo-name ws) remote))
        (t
         (format t "❌ Ошибка при pull из ~A: ~A~%" remote stdout))))
    ws))
