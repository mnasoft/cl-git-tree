(in-package :cl-git-tree/loc)

(defmethod repo-create ((ws <workspace>) (provider <provider>)  &key &allow-other-keys)
  (format nil "Метод REPO-CREATE неприменим для провайдера ~A."
          (class-name (class-of provider))))

(defmethod repo-create ((ws <workspace>) (provider <gitlab>)  &key &allow-other-keys)
  (cl-git-tree/git-utils:git-run (<workspace>-path ws)
                                 "remote" "add" (<location>-id provider)
                                 (<location>-url-git provider))
  (cl-git-tree/git-utils:git-run (<workspace>-path ws)
                                 "push" (<location>-id provider) "HEAD"))

(defmethod repo-create ((ws <workspace>) (provider <github>) &key &allow-other-keys)
  (cl-git-tree/git-utils:git-run (<workspace>-path ws)
                                 "remote" "add" (<location>-id provider)
                                 (<location>-url-git provider))
  (cl-git-tree/git-utils:git-run (<workspace>-path ws)
                                 "push" (<location>-id provider) "HEAD"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod repo-create ((ws <workspace>) (provider <github>) 
                        &key private &allow-other-keys)
  "Создать новый репозиторий на GitHub через CLI gh."
  (let* ((repo (repo-name ws))
         ;;(desc (or (<workspace>-description ws) repo))
         (root (git-root ws))
         (privacy-flag (if private "--private" "--public")))
    (multiple-value-bind (stdout stderr code)
        (cl-git-tree/shell-utils:shell-run
         root "gh" "repo" "create" repo
         privacy-flag
         "--source" (namestring root)
         "--remote" (<location>-id provider))
      (declare (ignore stderr))
      (cond
        ((zerop code)
         (format t "✅ Репозиторий ~A создан на GitHub (~A) и привязан к workspace ~A~%"
                 repo (if private "private" "public") root))
        (t
         (format t "❌ Ошибка при создании репозитория на GitHub (код ~A): ~A~%"
                 code stdout))))
    ws))

(defmethod repo-create ((ws <workspace>) (provider <gitlab>) 
                        &key private &allow-other-keys)
  "Создать новый репозиторий на GitHub через CLI gh."
  (let* ((repo (repo-name ws))
         ;;(desc (or (<workspace>-description ws) repo))
         (root (git-root ws))
         (privacy-flag (if private "--private" "--public")))
    (multiple-value-bind (stdout stderr code)
        (cl-git-tree/shell-utils:shell-run
         root
         "gh" "repo" "create" repo
         privacy-flag
         "--source" (namestring root)
         "--remote" (<location>-id provider))
      (declare (ignore stderr))
      (cond
        ((zerop code)
         (format t "✅ Репозиторий ~A создан на GitLab (~A) и привязан к workspace ~A~%"
                 repo (if private "private" "public") root))
        (t
         (format t "❌ Ошибка при создании репозитория на GitHub (код ~A): ~A~%"
                 code stdout))))
    ws))
