(in-package :cl-git-tree/loc)

(defun remote-import-cleanup-dir (ws provider &key (remote-name "lc-import") verbose)
  "Удаляет каталог временного remote после отключения. Возвращает T если удалено, NIL если не найден."
  (let* ((url-xz (and provider (<location>-url-xz provider)))
         (repo-dir (or (git-root ws)
                       (<workspace>-path ws)))
         (repo-name (and repo-dir (cl-git-tree/fs:repo-name repo-dir)))
         (candidate (and url-xz repo-name (merge-pathnames (format nil "~A.git/" repo-name) (uiop:ensure-directory-pathname url-xz)))))
    (cond
      ((not candidate)
       (when verbose (format t "  ⚠️  Не удалось вычислить путь для удаления каталога remote: ~A~%" remote-name))
       nil)
      ((not (probe-file candidate))
       (when verbose (format t "  ⚠️  Каталог remote не найден: ~A~%" candidate))
       nil)
      (t
       (when verbose (format t "  🗑️  Удаляю каталог remote: ~A~%" candidate))
      (cl-git-tree/fs:delete-directory-tree candidate)
       t))))
