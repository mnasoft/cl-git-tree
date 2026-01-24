(in-package :cl-git-tree/commands/transport)

(defun print-transport-export-help ()
  "Справка по git-tree transport export."
  (format t "Экспорт репозиториев в tar.xz архивы.~%~%")
  (format t "Использование:~%")
  (format t "  git-tree transport export [--days N] [--verbose] [--help]~%~%")
  (format t "Опции:~%")
  (format t "  --days N     Архивировать только репозитории с коммитами не старее N дней (по умолчанию 7)~%")
  (format t "  --verbose    Подробный вывод~%")
  (format t "  --help       Показать эту справку~%~%")
  (format t "Примечания:~%")
  (format t "  Архивы создаются для каждого провайдера с заданным :url-xz. Если :url-xz = NIL, провайдер пропускается.~%~%")
  (format t "Примеры:~%")
  (format t "  git-tree transport export~%")
  (format t "  git-tree transport export --days 1~%")
  (format t "  git-tree transport export --days 3000~%")
  (format t "  git-tree transport export --verbose~%"))

(defun transport-export-repo (repo-dir days-filter verbose)
  "Выполняет экспорт tar.xz для одного репозитория REPO-DIR.

Возвращает количество созданных архивов для данного репозитория."
  (let ((ws (cl-git-tree/loc:make-workspace repo-dir)))
    (cl-git-tree/loc:repo-transport-export-all 
     ws 
     :days-filter days-filter 
     :verbose verbose)))


(defun transport-export (args)
  "Создаёт tar.xz-архивы для найденных репозиториев с учётом опций --days и --verbose.

ARGS — список аргументов после слова export."
  (let ((days-filter 7)
        (processed 0)
        (archived 0)
        (verbose (member "--verbose" args :test #'string=)))

    (when (member "--help" args :test #'string=)
      (print-transport-export-help)
      (return-from transport-export))

    ;; Парсим аргументы один раз
    (loop for (arg val) on args by #'cddr
          do (when (string= arg "--days")
               (setf days-filter (parse-integer val :junk-allowed t))))

    (let ((ws (cl-git-tree/loc:make-workspace ".")))
      (unless verbose
        (format t "~A Архивирование репозиториев (--days ~A)...~%" (cl-git-tree/loc:find-emo ws "fs archive") days-filter)))

    ;; Обход репозиториев в стиле with-repo
    (flet ((export-one (repo-dir _args)
             (declare (ignore _args))
             (incf processed)
             (incf archived (transport-export-repo repo-dir days-filter verbose))))
      (cl-git-tree/fs:with-repo #'export-one args))

    (unless verbose
      (format t "~%=== Архивировано: ~A из ~A ===~%" archived processed))))
