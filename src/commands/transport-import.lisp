(in-package :cl-git-tree/commands/transport)

;; Импорт изменений для одного репозитория
(defun transport-import-repo (repo-dir verbose)
  "Импортирует изменения из tar.xz для одного репозитория REPO-DIR. Возвращает количество успешных импортов."
  (let* ((ws (cl-git-tree/loc:make-workspace repo-dir))
         (repo-name (cl-git-tree/fs:repo-name repo-dir))
         (provider-locs (cl-git-tree/loc:repo-providers ws))
         (imported 0))
    (format t "ws ~S~%provider-locs ~S~%verbose ~S~%" ws provider-locs verbose)
    (when verbose
      (format t "\nРепозиторий: ~A~%" repo-name))
    (when provider-locs
      (dolist (loc provider-locs)
        (when (cl-git-tree/loc:repo-transport-import ws loc :verbose verbose)
          (incf imported))))
    imported))

;; Главная команда: обход всех репозиториев и импорт
(defun transport-import (args)
  "Импортирует изменения из tar.xz архивов для найденных репозиториев с учётом опций --verbose.
ARGS — список аргументов после слова import."
  (format t "0000:~%")
  (let ((processed 0)
        (imported 0)
        (verbose (member "--verbose" args :test #'string=)))
    (unless verbose
      (format t "⬇ Импорт изменений из архивов для всех репозиториев...~%"))
    (flet ((import-one (repo-dir _args)
             (declare (ignore _args))
             (incf processed)
             (incf imported (transport-import-repo repo-dir verbose))))
      (cl-git-tree/fs:with-repo #'import-one args))
    (unless verbose
      (format t "~%=== Импортировано: ~A из ~A ===~%" imported processed))))
