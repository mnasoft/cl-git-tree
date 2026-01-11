(in-package :cl-git-tree/loc)

(defmethod repo-transport-export ((ws <workspace>) (provider <provider>)
     &key (days-filter 30) verbose &allow-other-keys)
  "Создаёт tar.xz‑архив для WORKSPACE в каталоге :url-xz провайдера PROVIDER.

Возвращает количество созданных архивов (0 или 1). Выводит информацию
в stdout аналогично команде git-tree transport export.

DAYS-FILTER пока не используется непосредственно в этом методе,
но передаётся для согласованности API и возможных будущих стратегий."
  (declare (ignore days-filter))
  (let* ((repo-dir (or (git-root ws)
                       (<workspace>-path ws)))
         (repo-name (and repo-dir (cl-git-tree/fs:repo-name repo-dir)))
         (url-xz (and provider (<location>-url-xz provider)))
         (prov-symbol (and provider (<location>-provider provider)))
         (archived 0))
    (when (and repo-dir url-xz)
      (multiple-value-bind (archive-name output-dir)
          (cl-git-tree/fs:create-tar-xz-archive
           repo-dir
           (uiop:ensure-directory-pathname
             (expand-path ws url-xz)))
        (when archive-name
          (incf archived)
          (unless verbose
            (format t "~A ~A (~A). Архив создан: ~A → ~A~%"
                    (find-emo ws "success")
                    repo-name prov-symbol archive-name output-dir)))))
    (when (and verbose (not url-xz))
      (format t "~A  Локация ~A (провайдер ~A) не имеет :url-xz~%"
              (find-emo ws "warning")
              (<location>-id provider)
              prov-symbol))
    archived))
