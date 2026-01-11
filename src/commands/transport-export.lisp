(in-package :cl-git-tree/commands/transport)

(defun transport-export-repo (repo-dir days-filter verbose)
  "Выполняет экспорт tar.xz для одного репозитория REPO-DIR.

Возвращает количество созданных архивов для данного репозитория."
  (let* ((ws (cl-git-tree/loc:make-workspace repo-dir))
         (repo-name (cl-git-tree/fs:repo-name repo-dir))
         ;; Список локаций-провайдеров для данного репозитория
         (provider-locs (cl-git-tree/loc:repo-providers ws))
         (skip nil)
         (skip-reason nil)
         (archived 0))

    ;; Проверяем чистоту репозитория
    (unless (cl-git-tree/loc:repo-is-clean-p ws)
      (setf skip t skip-reason "незакоммиченные изменения"))

    ;; Проверяем дату последнего коммита
    (when (and (not skip) days-filter)
      (let ((days (cl-git-tree/loc:days-since-last-commit ws)))
        (if days
            (if (> days days-filter)
                (progn
                  (setf skip t skip-reason (format nil "коммит ~A дней назад" days)))
                (when verbose
                  (format t "~%Репозиторий: ~A~%" repo-name)
                  (format t "  ~A Последний коммит ~A дней назад~%" 
                          (cl-git-tree/loc:find-emo ws "success")
                          days)))
            (progn
              (setf skip t skip-reason "не удалось определить дату коммита")))))

    ;; Выводим причину пропуска, если есть
    (when (and verbose skip)
      (unless skip-reason
        (setf skip-reason "неизвестная причина"))
      (format t "~%Репозиторий: ~A~%" repo-name)
      (format t "  ~A  Пропущено: ~A~%" (cl-git-tree/loc:find-emo ws "warning") skip-reason))

    ;; Архивируем для каждой найденной локации-провайдера через generic-функцию
    (when (and (not skip) provider-locs)
      (dolist (loc provider-locs)
        (incf archived
              (cl-git-tree/loc:repo-transport-export
               ws loc :verbose verbose :days-filter days-filter))))

    archived))


(defun transport-export (args)
  "Создаёт tar.xz-архивы для найденных репозиториев с учётом опций --days и --verbose.

ARGS — список аргументов после слова export."
  (let ((days-filter 30)
        (processed 0)
        (archived 0)
        (verbose (member "--verbose" args :test #'string=)))

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
