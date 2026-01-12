(in-package :cl-git-tree/loc)

;; cleanup-remote-dir заменён на keep-remote-dir (по умолчанию NIL, если T — каталог не удаляется)
(defmethod repo-transport-import ((ws <workspace>) (provider <provider>)
                                  &key verbose keep-remote-dir delete-archive &allow-other-keys)
  "Импортирует изменения из tar.xz архива для WORKSPACE и PROVIDER.
Распаковывает архив из :url-xz провайдера в рабочий каталог репозитория.
Если keep-remote-dir=T, временный каталог remote не удаляется."
  (let* ((repo-dir (or (git-root ws)
                       (<workspace>-path ws)))
         (repo-name (and repo-dir (cl-git-tree/fs:repo-name repo-dir)))
         (url-xz (and provider (<location>-url-xz provider)))
         (prov-symbol (and provider (<location>-provider provider))))
    (cond
      ((not repo-dir)
       (when verbose
         (format t "~A Не найден путь к репозиторию для ~A~%"
                 (find-emo ws "warning")
                 repo-name))
       (values nil nil))
      ((not url-xz)
       (when verbose
         (format t "~A Локация ~A (провайдер ~A) не имеет :url-xz~%"
                 (find-emo ws "warning")
                 (<location>-id provider) prov-symbol))
       (values nil nil))
      (t
       (if (repo-transport-unpack ws provider :verbose verbose)
           ;; Подключаем временный remote (например, "<loc>-import"), делаем pull, затем отключаем
           (let ((tmp-remote (format nil "~A-import" (<location>-id provider))))
             (if (remote-import-connect ws provider :remote-name tmp-remote :verbose verbose)
                 (progn
                   ;; Выполняем pull с использованием временного remote, только fast-forward
                   (multiple-value-bind (ignore-ws success)
                       (cl-git-tree/loc:repo-pull ws provider :remote tmp-remote :branch "master" :ff-only t)
                     (declare (ignore ignore-ws))
                     ;; Отключаем временный remote
                     (remote-import-disconnect ws provider :remote-name tmp-remote :verbose verbose)
                     ;; Удаляем каталог временного remote только при успехе и если не keep-remote-dir
                     (unless (or keep-remote-dir (not success))
                       (remote-import-cleanup-dir ws provider :verbose verbose))
                     ;; Удаляем архив (опционально)
                     (when delete-archive
                       (remote-import-delete-archive ws provider :verbose verbose))
                     (values success t)))
               ;; Не удалось подключить временный remote
               (values nil nil)))
           ;; Распаковка архива не удалась
           (values nil nil))))))
