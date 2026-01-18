(in-package :cl-git-tree/loc)

(defmethod repo-push-all ((ws <workspace>) &key &allow-other-keys)
  "Выполнить git push для всех зарегистрированных провайдеров рабочего пространства.
Определяет текущую ветку автоматически. Возвращает WS."
  (let* ((root (git-root ws))
         (branch (and root (cl-git-tree/git-utils:current-branch root)))
         (providers (repo-providers ws)))
    (dolist (provider providers)
      (handler-case
          (repo-push ws provider :branch branch)
        (error (e)
          (format t "~A [~A] Ошибка: ~A~%"
                  (find-emo ws "error")
                  (<location>-id provider)
                  e))))
    ws))
