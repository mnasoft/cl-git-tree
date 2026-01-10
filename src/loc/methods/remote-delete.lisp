(in-package :cl-git-tree/loc)

(defmethod remote-delete ((ws <workspace>) (provider <provider>)  &key &allow-other-keys)
  (format nil "Метод REMOTE-DELETE неприменим для провайдера ~A."
          (class-name (class-of provider))))

(defmethod remote-delete ((ws <workspace>) (provider <github>)
                        &key (yes t) remote-only &allow-other-keys)
  "Удалить репозиторий на GitHub через CLI gh."
  (let* ((repo (repo-name ws))
         (root (git-root ws))
         (args (list "gh" "repo" "delete" repo)))
    (when yes (setf args (append args (list "--yes"))))
    
    (multiple-value-bind (stdout stderr code)
        (uiop:run-program args
                          :directory root
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (cond
        ((zerop code)
         (format t "✅ [~A] Репозиторий ~A удалён на GitHub~%" 
                 (<location>-id provider) repo)
         (unless remote-only
           (cl-git-tree/git-utils:git-run root "git" "remote" "remove" 
                                          (<location>-id provider))))
        (t
         (format t "❌ [~A] Ошибка при удалении ~A (код ~A): ~A~%"
                 (<location>-id provider) repo code (or stderr stdout)))))
    ws))

(defmethod remote-delete ((ws <workspace>) (provider <gitlab>)
                        &key (yes t) remote-only &allow-other-keys)
  "Удалить репозиторий на GitLab через CLI glab."
  (let* ((repo (repo-name ws))
         (root (git-root ws))
         (args (list "glab" "repo" "delete" repo)))
    (when yes (setf args (append args (list "--yes"))))
    
    (multiple-value-bind (stdout stderr code)
        (uiop:run-program args
                          :directory root
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (cond
        ((zerop code)
         (format t "✅ [~A] Репозиторий ~A удалён на GitLab~%" 
                 (<location>-id provider) repo)
         (unless remote-only
           (cl-git-tree/git-utils:git-run root "git" "remote" "remove" 
                                          (<location>-id provider))))
        (t
         (format t "❌ [~A] Ошибка при удалении ~A (код ~A): ~A~%"
                 (<location>-id provider) repo code (or stderr stdout)))))
    ws))

(defmethod remote-delete ((ws <workspace>) (provider <local>)
                          &key (yes t) remote-only &allow-other-keys)
  "Удалить bare-репозиторий из локальной директории."
  (declare (ignore yes))

  (flet ((force-delete-directory (dir)
           "Удалить DIR, предварительно снимая атрибут read-only под Windows/MSYS2."
           (handler-case
               (cl-git-tree/fs:delete-directory-tree dir)
             (file-error (e)
	       ;; На Windows pack-файлы в .git/objects иногда помечаются read-only
	       ;; (или удерживаются индексатором). Снимаем атрибуты и пробуем ещё раз.
               (when (member (<workspace>-os-type ws) '(:windows :msys2))
                 (ignore-errors
                  (uiop:run-program (list "cmd.exe" "/c" "attrib" "-R" "/S" "/D"
                                          (uiop:native-namestring dir))
                                    :ignore-error-status t))
                 (cl-git-tree/fs:delete-directory-tree dir)
                 (return-from force-delete-directory t))
               (error e)))))

    (let* (
	   ;; Expand user shorthand (e.g., "~") to a physical directory pathname.
           (base (uiop:ensure-directory-pathname
                  (uiop:ensure-absolute-pathname (<location>-url-git provider)
                                                 (user-homedir-pathname))))
           (target (uiop:ensure-directory-pathname
                    (merge-pathnames (format nil "~A.git/" (repo-name ws)) base))))
      (when (uiop:directory-exists-p target)
        (force-delete-directory target)
        (format t "🗑️  [~A] Bare-репозиторий удалён: ~A~%"
                (<location>-id provider) (uiop:native-namestring target))
        (remote-remove ws provider)
        )
      (unless remote-only nil ;; ToDo
        )
      ws)))

(defmethod remote-delete ((ws <workspace-msys2>) (provider <local>)
                          &key (yes t) remote-only &allow-other-keys)
  "Удалить bare-репозиторий из локальной директории через rm -r."
  (declare (ignore yes))
  (let ((root (git-root ws))
        (target (remote-url ws provider)))
    ;; Удалить bare-репозиторий через rm -r
    (cl-git-tree/shell-utils:shell-run-single "." "rm" "-rf" target)
    ;; Удалить remote из конфигурации git
    (remote-remove ws provider)
    (unless remote-only nil ;; ToDo
      )
    ws))
