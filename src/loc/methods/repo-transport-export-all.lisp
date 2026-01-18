(in-package :cl-git-tree/loc)

(defmethod repo-transport-export-all ((ws <workspace>)
     &key (days-filter 30) verbose &allow-other-keys)
  "Выполняет экспорт tar.xz для всех провайдеров в WORKSPACE.

Определяет список провайдеров, выполняет проверки (чистота репозитория,
дата последнего коммита) и вызывает repo-transport-export для каждой
найденной локации-провайдера.

Возвращает количество созданных архивов (целое число)."
  (let* ((repo-dir (or (git-root ws)
                       (<workspace>-path ws)))
         (repo-name (and repo-dir (cl-git-tree/fs:repo-name repo-dir)))
         ;; Список локаций-провайдеров для данного репозитория
         (provider-locs (repo-providers ws))
         (skip nil)
         (skip-reason nil)
         (archived 0))

    ;; Проверяем чистоту репозитория
    (unless (repo-is-clean-p ws)
      (setf skip t skip-reason "незакоммиченные изменения"))

    ;; Проверяем дату последнего коммита
    (when (and (not skip) days-filter)
      (let ((days (days-since-last-commit ws)))
        (if days
            (if (> days days-filter)
                (progn
                  (setf skip t skip-reason (format nil "коммит ~A дней назад" days)))
                (when verbose
                  (format t "~%Репозиторий: ~A~%" repo-name)
                  (format t "  ~A Последний коммит ~A дней назад~%" 
                          (find-emo ws "success")
                          days)))
            (progn
              (setf skip t skip-reason "не удалось определить дату коммита")))))

    ;; Выводим причину пропуска, если есть
    (when (and verbose skip)
      (unless skip-reason
        (setf skip-reason "неизвестная причина"))
      (format t "~%Репозиторий: ~A~%" repo-name)
      (format t "  ~A  Пропущено: ~A~%" (find-emo ws "warning") skip-reason))

    ;; Архивируем для каждой найденной локации-провайдера через repo-transport-export
    (when (and (not skip) provider-locs)
      (dolist (loc provider-locs)
        (incf archived
              (repo-transport-export ws loc :verbose verbose :days-filter days-filter))))

    archived))
