(in-package :cl-git-tree/loc)

(defmethod remote-create ((ws <workspace>) (provider <provider>)  &key &allow-other-keys)
  (format nil "Метод REMOTE-CREATE неприменим для провайдера ~A."
          (class-name (class-of provider))))

(defmethod remote-create ((ws <workspace>) (provider <github>) 
                        &key private &allow-other-keys)
  "Создать новый репозиторий на GitHub через CLI gh."
  (let* ((repo (repo-name ws))
         (root (git-root ws))
         (privacy-flag (if private "--private" "--public")))
    (multiple-value-bind (stdout stderr code)
        (uiop:run-program (list "gh" "repo" "create" repo
                                privacy-flag
                                "--source" (namestring root)
                                "--remote" (<location>-id provider))
                          :directory root
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (cond
        ((zerop code)
         (format t "✅ [~A] Репозиторий ~A создан на GitHub (~A)~%"
                 (<location>-id provider) repo (if private "private" "public")))
        (t
         (format t "❌ [~A] Ошибка при создании репозитория на GitHub (код ~A): ~A~%"
                 (<location>-id provider) code (or stderr stdout)))))
    ws))

(defmethod remote-create ((ws <workspace>) (provider <gitlab>) 
                        &key private &allow-other-keys)
  "Создать новый репозиторий на GitLab через CLI glab."
  (let* ((repo (repo-name ws))
         (root (git-root ws))
         (privacy-flag (if private "--private" "--public")))
    (multiple-value-bind (stdout stderr code)
        (uiop:run-program (list "glab" "repo" "create" repo
                                privacy-flag
                                "--source" (namestring root)
                                "--remote" (<location>-id provider))
                          :directory root
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (cond
        ((zerop code)
         (format t "✅ [~A] Репозиторий ~A создан на GitLab (~A)~%"
                 (<location>-id provider) repo (if private "private" "public")))
        (t
         (format t "❌ [~A] Ошибка при создании репозитория на GitLab (код ~A): ~A~%"
                 (<location>-id provider) code (or stderr stdout)))))
    ws))

(defmethod remote-create ((ws <workspace>) (provider <local>)
                          &key &allow-other-keys)
  "Создать bare-репозиторий для WORKSPACE под локальным провайдером.
Если цель уже существует — пропустить; иначе выполнить `git clone --bare`
из рабочего каталога в каталог провайдера (например ~/.git-tree/git/<id>/REPO.git)."
  (let* ((repo (repo-name ws))
         (base (uiop:ensure-directory-pathname (<location>-url-git provider)))
         (target (merge-pathnames (format nil "~A.git" repo) base)))
    (cond
      ((probe-file target)
       (format t "⚠️ [~A] Репозиторий ~A уже существует: ~A~%" 
               (<location>-id provider) repo target))
      (t
       (ensure-directories-exist target)
       (multiple-value-bind (out err code)
           (cl-git-tree/git-utils:git-run 
            (<workspace>-path ws) 
            "clone" "--bare" "." 
            (cl-git-tree/git-utils:normalize-path-for-git (namestring target)))
         (declare (ignore out))
         (if (zerop code)
             (progn 
               (format t "🧬 [~A] Bare-репозиторий создан: ~A~%" 
                       (<location>-id provider) target)
               (remote-add ws provider))
             (format t "❌ [~A] Ошибка создания ~A: ~A~%" 
                     (<location>-id provider) repo err)))))
    ws))

