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
  (let* ((repo (repo-name ws))
         (root (git-root ws))
         (base (uiop:ensure-directory-pathname (<location>-url-git provider)))
         (target (merge-pathnames (format nil "~A.git" repo) base)))
    (when (probe-file target)
      (cl-git-tree/shell-utils:shell-run-single
       "/"
       "rm" "-rf" (namestring target))
      (format t "🗑️ Bare-репозиторий удалён: ~A~%" target))
    (unless remote-only
      (cl-git-tree/shell-utils:shell-run-single
       root "git" "remote" "remove" (<location>-id provider)))
    ws))
