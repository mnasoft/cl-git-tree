(in-package :cl-git-tree/loc)




(defmethod repo-transport-import ((ws <workspace>) (provider <provider>) &key verbose &allow-other-keys)
  "Импортирует изменения из tar.xz архива для WORKSPACE и PROVIDER.\nРаспаковывает архив из :url-xz провайдера в рабочий каталог репозитория."
  (let* ((repo-dir (or (git-root ws)
                       (<workspace>-path ws)))
         (repo-name (and repo-dir (cl-git-tree/fs:repo-name repo-dir)))
         (url-xz (and provider (<location>-url-xz provider)))
         (prov-symbol (and provider (<location>-provider provider))))
    (cond
      ((not repo-dir)
       (when verbose
         (format t "  ⚠️  Не найден путь к репозиторию для ~A~%" repo-name))
       nil)
      ((not url-xz)
       (when verbose
         (format t "  ⚠️  Локация ~A (провайдер ~A) не имеет :url-xz~%"
                 (<location>-id provider)
                 prov-symbol))
       nil)
      ((not (probe-file url-xz))
       (when verbose
         (format t "  ⚠️  Архив не найден: ~A~%" url-xz))
       nil)
      (t
       (let ((cmd (format nil "tar -xJf ~A -C ~A" url-xz repo-dir)))
         (when verbose
           (format t "  ⏳ Импорт: ~A → ~A~%" url-xz repo-dir))
         (multiple-value-bind (output error-output exit-code)
             (uiop:run-program cmd :output :string :error-output :string :ignore-error-status t)
           (declare (ignore output error-output))
           (if (zerop exit-code)
               (progn
                 (when verbose
                   (format t "  ✅ Импорт завершён: ~A~%" repo-name))
                 t)
               (progn
                 (when verbose
                   (format t "  ❌ Ошибка при импорте: ~A~%" repo-name))
                 nil))))))))
