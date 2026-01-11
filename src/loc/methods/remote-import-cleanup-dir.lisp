(in-package :cl-git-tree/loc)


(defmethod remote-import-cleanup-dir ((ws <workspace>)
                                      (provider <provider>)
                                      &key verbose &allow-other-keys )
  "Удаляет каталог временного remote после отключения. Возвращает T если удалено, NIL если не найден."
  (let* ((url-xz (and provider (<location>-url-xz provider)))
         (repo-dir (or (git-root ws)
                       (<workspace>-path ws)))
         (repo-name (and
                     repo-dir
                     (cl-git-tree/fs:repo-name repo-dir)))
         (candidate (and
                     url-xz
                     repo-name
                     (merge-pathnames (format nil "~A.git/" repo-name)
                                      (uiop:ensure-directory-pathname url-xz))))
         (remote-name (concatenate 'string repo-name "-import")))
    (cond
      ((not candidate)
       (when verbose
         (format t "~A Не удалось вычислить путь для удаления каталога remote: ~A~%"
                 (find-emo ws "warning")
                 remote-name))
       nil)
      ((not (probe-file candidate))
       (when verbose (format t "~A Каталог remote не найден: ~A~%"
                             (find-emo ws "warning")
                             candidate))
       nil)
      (t
       (when verbose (format t "  🗑️  Удаляю каталог remote: ~A~%" candidate))
       (cl-git-tree/fs:delete-directory-tree candidate)
       t))))
