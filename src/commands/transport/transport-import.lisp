(in-package :cl-git-tree/commands/transport)

;; Импорт изменений для одного репозитория
(defun transport-import-repo (repo-dir verbose &key keep-remote-dir delete-archive)
  "Импортирует изменения из tar.xz для одного репозитория REPO-DIR.
Возвращает два значения: количество успешных pull (FF) и количество неуспешных pull (FF-ошибок)."
  (let* ((ws (cl-git-tree/loc:make-workspace repo-dir))
         (repo-name (cl-git-tree/fs:repo-name repo-dir))
         (provider-locs (cl-git-tree/loc:repo-providers ws))
         (ff-success 0)
         (ff-failed 0))
    (when verbose
      (format t "~%Репозиторий: ~A~%" repo-name))
    (when provider-locs
      (dolist (loc provider-locs)
        (multiple-value-bind (success attempted)
            (cl-git-tree/loc:repo-transport-import ws loc :verbose verbose :keep-remote-dir keep-remote-dir :delete-archive delete-archive)
          (when attempted
            (if success
                (incf ff-success)
              (incf ff-failed))))))
    (values ff-success ff-failed)))

;; Главная команда: обход всех репозиториев и импорт
(defun transport-import (args)
  "Импортирует изменения из tar.xz архивов для найденных репозиториев с учётом опций --verbose, --keep-remote-dir, --delete-archive."
    (let ((processed 0)
        (ff-success-total 0)
        (ff-failed-total 0)
        (verbose (member "--verbose" args :test #'string=))
        (keep-remote-dir (member "--keep-remote-dir" args :test #'string=))
        (delete-archive (member "--delete-archive" args :test #'string=)))
    (unless verbose
      (format t "⬇ Импорт изменений из архивов для всех репозиториев...~%"))
    (flet ((import-one (repo-dir _args)
             (declare (ignore _args))
             (incf processed)
             (multiple-value-bind (succ fail)
                 (transport-import-repo repo-dir verbose
                                        :keep-remote-dir keep-remote-dir
                                        :delete-archive delete-archive)
               (incf ff-success-total succ)
               (incf ff-failed-total fail))))
      (cl-git-tree/fs:with-repo #'import-one args))
    (unless verbose
      (format t "~%=== Просмотрено каталогов: ~A; FF-успехов: ~A; FF-ошибок: ~A ===~%"
              processed ff-success-total ff-failed-total))))
