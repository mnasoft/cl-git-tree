(in-package :cl-git-tree/loc)

(defmethod clone ((provider <provider>) target-path)
  (format nil "Метод CLONE неприменим для провайдера ~A."
          (class-name (class-of provider))))

(defmethod clone ((provider <local>) target-path)
  "Клонировать локальный репозиторий как bare в TARGET-PATH."
  (let* ((repo-dir (cl-git-tree/loc:<location>-url-git provider))
         (repo-name (cl-git-tree/fs:repo-name repo-dir))
         (target (merge-pathnames (format nil "~A.git" repo-name)
                                  (uiop:ensure-directory-pathname target-path))))
    (cond
      ;; если уже существует — пропускаем
      ((probe-file target)
       (format t "⚠ ~A: уже существует ~A~%" repo-name target))
      (t
       (ensure-directories-exist target)
       (multiple-value-bind (out err code)
           (cl-git-tree/git-utils:git-run repo-dir "clone" "--bare" "." (namestring target))
         (declare (ignore out))
         (if (zerop code)
             (format t "✔ ~A → ~A~%" repo-name target)
             (format t "❌ ~A: clone failed~%~A~%" repo-name err)))))))
