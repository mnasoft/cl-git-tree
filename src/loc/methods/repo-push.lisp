(in-package :cl-git-tree/loc)

(defmethod repo-push ((ws <workspace>) (provider <provider>) 
                      &key (remote (<location>-id provider)) branch force tags set-upstream
                      &allow-other-keys)
  "Выполнить git push."
  (let ((root (git-root ws))
        (args (list "push" remote)))
    ;; Добавляем опции только если они заданы
    (when set-upstream (push "--set-upstream" args))
    (when force        (push "--force"        args))
    (when tags         (push "--tags"         args))
    (when branch       (setf args (append args (list branch))))
    
    (multiple-value-bind (stdout stderr code)
        (apply #'cl-git-tree/git-utils:git-run root args)
      (cond
        ((zerop code)
         (format t "✅ [~A] Push ~A~A успешно~%"
                 remote (repo-name ws) (if branch (format nil "/~A" branch) "")))
        (t
         (format t "❌ [~A] Ошибка push ~A: ~A~%" 
                 remote (repo-name ws) 
                 (or stderr stdout "неизвестная ошибка")))))
    ws))
