(in-package :cl-git-tree/loc)

(defmethod repo-delete ((ws <workspace>) (provider <provider>)  &key &allow-other-keys)
  (format nil "Метод REPO-DELETE неприменим для провайдера ~A."
          (class-name (class-of provider))))

(defmethod repo-delete ((provider <github>) (ws <workspace>)
                        &key (yes t) remote-only &allow-other-keys)
  "Удалить репозиторий на GitHub через CLI gh."
  (let* ((repo (repo-name ws))
         (root (git-root ws))
         (yes-flag (when yes "--yes")))
    (multiple-value-bind (stdout stderr code)
        (cl-git-tree/shell-utils:shell-run-single root
                                           "gh" "repo" "delete" repo
                                           yes-flag)
      (declare (ignore stderr))
      (cond
        ((zerop code)
         (format t "🗑️ Репозиторий ~A удалён на GitHub~%" repo)
         (unless remote-only
           ;; можно подчистить локальный remote
           (cl-git-tree/shell-utils:shell-run-single
            root "git" "remote" "remove" (<location>-id provider))))
        (t
         (format t "❌ Ошибка при удалении репозитория (код ~A): ~A~%"
                 code stdout))))
    ws))

(defmethod repo-delete ((ws <workspace>) (provider <gitlab>)
                        &key (yes t) remote-only &allow-other-keys)
  "Удалить репозиторий на GitLab через CLI glab."
  (let* ((repo (repo-name ws))
         (root (git-root ws))
         (yes-flag (when yes "--yes")))
    (multiple-value-bind (stdout stderr code)
        (cl-git-tree/shell-utils:shell-run-single root
                                           "glab" "repo" "delete" repo
                                           yes-flag)
      (declare (ignore stderr))
      (cond
        ((zerop code)
         (format t "🗑️ Репозиторий ~A удалён на GitLab~%" repo)
         (unless remote-only
           ;; можно подчистить локальный remote
           (cl-git-tree/shell-utils:shell-run-single
            root "git" "remote" "remove" (<location>-id provider))))
        (t
         (format t "❌ Ошибка при удалении репозитория (код ~A): ~A~%"
                 code stdout))))
    ws))
