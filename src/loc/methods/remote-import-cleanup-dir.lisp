(in-package :cl-git-tree/loc)


(defmethod remote-import-cleanup-dir ((ws <workspace>)
                                      (provider <provider>)
                                      &key verbose &allow-other-keys )
  "@b(Назначение:) Удаляет каталог временного remote после @code(remote-import-disconnect).
@b(Пример:) @begin[lang=lisp](code)
 (remote-import-cleanup-dir ws provider :verbose t)
@end(code)
Возвращает @code(T), если каталог найден и удалён, иначе @code(NIL)."
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
       (when verbose (format t "  ~A  Удаляю каталог remote: ~A~%" (find-emo ws "fs delete") candidate))
       (cl-git-tree/fs:delete-directory-tree candidate)
       t))))
