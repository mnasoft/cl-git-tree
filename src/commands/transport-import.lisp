(in-package :cl-git-tree/commands/transport)

;; Импорт изменений для одного репозитория
(defun transport-import-repo (repo-dir verbose &key keep-remote-dir delete-archive)
  "Импортирует изменения из tar.xz для одного репозитория REPO-DIR. Возвращает количество успешных импортов."
  (let* ((ws (cl-git-tree/loc:make-workspace repo-dir))
         (repo-name (cl-git-tree/fs:repo-name repo-dir))
         (provider-locs (cl-git-tree/loc:repo-providers ws))
         (imported 0))
    (when verbose
      (format t "~%Репозиторий: ~A~%" repo-name))
    (when provider-locs
      (dolist (loc provider-locs)
        (when (cl-git-tree/loc:repo-transport-import ws loc :verbose verbose :cleanup-remote-dir (not keep-remote-dir) :delete-archive delete-archive)
          (incf imported))))
    imported))

;; Главная команда: обход всех репозиториев и импорт
(defun transport-import (args)
  "Импортирует изменения из tar.xz архивов для найденных репозиториев с учётом опций --verbose, --keep-remote-dir, --delete-archive."
    (let ((processed 0)
        (imported 0)
        (verbose (member "--verbose" args :test #'string=))
        (keep-remote-dir (member "--keep-remote-dir" args :test #'string=))
        (delete-archive (member "--delete-archive" args :test #'string=)))
    (unless verbose
      (format t "⬇ Импорт изменений из архивов для всех репозиториев...~%"))
    (flet ((import-one (repo-dir _args)
             (declare (ignore _args))
             (incf processed)
             (incf imported (transport-import-repo repo-dir verbose
                                                   :keep-remote-dir keep-remote-dir
                                                   :delete-archive delete-archive))))
      (cl-git-tree/fs:with-repo #'import-one args))
    (unless verbose
      (format t "~%=== Импортировано: ~A из ~A ===~%" imported processed))))
