(in-package :cl-git-tree/commands/transport)

(defun clean-tar-xz-archives (output-path)
  "Удаляет tar.xz архивы в каталоге output-path."
  (let* ((pattern (merge-pathnames #p"*.tar.xz" output-path))
         (archives (directory pattern))
         (deleted 0))
    (if archives
        (progn
          (dolist (file archives)
            (when (probe-file file)
              (delete-file file)
              (incf deleted)))
          (format t "🧹 Удалено архивов: ~A (путь ~A)~%" deleted output-path))
        (format t "Архивы не найдены в ~A~%" output-path))
    deleted))

(defun transport-clean ()
  "Очищает tar.xz-архивы во всех :url-xz для зарегистрированных локаций."
  (let ((total-deleted 0))
    (format t "🧹 Очистка архивов tar.xz из каталогов :url-xz всех провайдеров~%~%")
    (dolist (loc-key (cl-git-tree/loc:all-location-keys))
      (let* ((loc (cl-git-tree/loc:find-location loc-key))
             (url-xz (and loc (cl-git-tree/loc:<location>-url-xz loc))))
        (when url-xz
          (let* ((xz-dir (uiop:ensure-directory-pathname (cl-git-tree/fs:expand-home url-xz)))
                 (deleted (clean-tar-xz-archives xz-dir)))
            (incf total-deleted deleted)))))
    (format t "~%=== Итого удалено архивов: ~A ===~%" total-deleted)))
