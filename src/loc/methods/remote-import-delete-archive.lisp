(in-package :cl-git-tree/loc)

(defmethod remote-import-delete-archive ((ws <workspace>) (provider <provider>) &key verbose &allow-other-keys)
  "Удаляет tar.xz архив для WORKSPACE/PROVIDER. Возвращает T если удалено, NIL если не найден."
  (let* ((repo-dir (or (git-root ws)
                       (<workspace>-path ws)))
         (repo-name (and repo-dir (cl-git-tree/fs:repo-name repo-dir)))
         (url-xz (and provider (<location>-url-xz provider)))
         (tar-xz (and url-xz repo-name (concatenate 'string url-xz "/" repo-name ".tar.xz"))))
    (cond
      ((not tar-xz)
       (when verbose
         (format t "~A Не удалось вычислить путь к архиву для удаления~%"
                 (find-emo ws "warning")))
       nil)
      ((not (probe-file tar-xz))
       (when verbose (format t "~A Архив не найден: ~A~%"
                             (find-emo ws "warning")
                             tar-xz))
       nil)
      (t
       (when verbose (format t "  ~A  Удаляю архив: ~A~%" (find-emo ws "fs delete") tar-xz))
       (cl-git-tree/fs:delete-directory-tree tar-xz)
       t))))
