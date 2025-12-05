(in-package :cl-git-tree/loc)

(defmethod remote-create ((ws <workspace>) (provider <provider>)  &key &allow-other-keys)
  (format nil "Метод REMOTE-CREATE неприменим для провайдера ~A."
          (class-name (class-of provider))))

(defmethod remote-create ((ws <workspace>) (provider <github>) 
                        &key private &allow-other-keys)
  "Создать новый репозиторий на GitHub через CLI gh."
  (let* ((repo (repo-name ws))
         ;;(desc (or (<workspace>-description ws) repo))
         (root (git-root ws))
         (privacy-flag (if private "--private" "--public")))
    (multiple-value-bind (stdout stderr code)
        (cl-git-tree/shell-utils:shell-run-single
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

(defmethod remote-create ((ws <workspace>) (provider <gitlab>) 
                        &key private &allow-other-keys)
  "Создать новый репозиторий на GitLab через CLI glab."
  (let* ((repo (repo-name ws))
         ;;(desc (or (<workspace>-description ws) repo))
         (root (git-root ws))
         (privacy-flag (if private "--private" "--public")))
    (multiple-value-bind (stdout stderr code)
        (cl-git-tree/shell-utils:shell-run-single
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

(defmethod remote-create ((ws <workspace>) (provider <local>) &key &allow-other-keys)
  "Создать bare-репозиторий для WORKSPACE под локальным провайдером.
Если цель уже существует — пропустить; иначе выполнить `git clone --bare`
из рабочего каталога в каталог провайдера (например ~/.git-tree/git/<id>/REPO.git)."
  (let* ((repo (repo-name ws))
         (base (uiop:ensure-directory-pathname (<location>-url-git provider)))
         (target (merge-pathnames (format nil "~A.git" repo) base)))
    (cond
      ((probe-file target)
       (format t "⚠ ~A: уже существует: ~A~%" repo target))
      (t
       (ensure-directories-exist (pathname-directory target))
       (multiple-value-bind (out err code)
           (cl-git-tree/git-utils:git-run (<workspace>-path ws) "clone" "--bare" "." (namestring target))
         (declare (ignore out))
         (if (zerop code)
             (format t "✔ ~A → ~A~%" repo target)
             (format t "❌ ~A: clone failed: ~A~%" repo err)))))
  ws))
