(in-package :cl-git-tree/loc)

(defmethod remote-delete ((ws <workspace>) (provider <provider>)
                          &key &allow-other-keys)
  (format nil "Метод REMOTE-DELETE неприменим для провайдера ~A."
          (class-name (class-of provider))))

(defmethod remote-delete ((ws <workspace>) (provider <github>)
                          &key &allow-other-keys)
  "Удалить репозиторий на GitHub через CLI gh."
  (multiple-value-bind (stdout stderr code)
      (uiop:run-program (list "gh" "repo" "delete" (repo-name ws) "--yes")
                        :directory (git-root ws)
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (cond ((zerop code)
           (format t "✅ [~A] Репозиторий ~A удалён на GitHub~%" 
                   (<location>-id provider)
                   (repo-name ws))
           (remote-remove ws provider))
          (t
           (format t "❌ [~A] Ошибка при удалении ~A (код ~A): ~A~%"
                   (<location>-id provider)
                   (repo-name ws)
                   code
                   (or stderr stdout)))))
  ws)

(defmethod remote-delete ((ws <workspace>) (provider <gitlab>)
                          &key &allow-other-keys)
  "Удалить репозиторий на GitLab через CLI glab."
  (multiple-value-bind (stdout stderr code)
      (uiop:run-program (list "glab" "repo" "delete" (repo-name ws) "--yes")
                        :directory (git-root ws)
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (cond
      ((zerop code)
       (format t "✅ [~A] Репозиторий ~A удалён на GitLab~%" 
               (<location>-id provider)
               (repo-name ws))
       (remote-remove ws provider))
      (t
       (format t "❌ [~A] Ошибка при удалении ~A (код ~A): ~A~%"
               (<location>-id provider)
               (repo-name ws)
               code
               (or stderr stdout)))))
  ws)

(defmethod remote-delete ((ws <workspace>) (provider <local>)
                          &key &allow-other-keys)
  "Удалить bare-репозиторий из локальной директории."
  (let ((target (remote-url-full ws provider)))
    (when (uiop:directory-exists-p target)
      (cl-git-tree/fs:delete-directory-tree target)
      (format t "🗑️  [~A] Bare-репозиторий удалён: ~A~%"
              (<location>-id provider)
              (uiop:native-namestring target))
      (remote-remove ws provider))
    ws))

(defmethod remote-delete ((ws <workspace-msys2>) (provider <local>)
                          &key &allow-other-keys)
  "Удалить bare-репозиторий из локальной директории через rm -r."
  (cl-git-tree/shell-utils:shell-run-single "." "rm" "-rf" (remote-url ws provider))
  (format t "🗑️  [~A] Bare-репозиторий удалён: ~A~%"
          (<location>-id provider)
          (uiop:native-namestring
           (remote-url ws provider)))
  (remote-remove ws provider)
  ws)
