;;;; ./src/loc/methods/remote-remove.lisp

(in-package :cl-git-tree/loc)

(defmethod remote-remove ((ws <workspace>) (provider <provider>) &key &allow-other-keys)
  "Удалить отдаленный репозиторий для рабочего пространства WORKSPACE,
связанный с провайдером PROVIDER."
  (let* ((root (git-root ws))
         (remote-name (<location>-id provider))
         (args (list "remote" "remove" remote-name)))
    (multiple-value-bind (stdout stderr code)
        (apply #'cl-git-tree/git-utils:git-run root args)
      (cond
        ((zerop code)
         (format t "✅ Репозиторий ~A: remote '~A' удалён~%"
                 (repo-name ws) remote-name))
        (t
         (format t "❌ Ошибка при удалении remote '~A' из ~A: ~A~%"
                 remote-name (repo-name ws) (or stderr stdout)))))
    ws))

