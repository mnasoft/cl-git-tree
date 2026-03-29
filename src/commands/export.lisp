;;;; ./src/commands/export.lisp

(defpackage :cl-git-tree/commands/export
  (:use :cl)
  (:export cmd-export
           export-repo))

(in-package :cl-git-tree/commands/export)

(defun print-export-help ()
  "Справка по git-tree export."
  (format t "Экспорт репозиториев в tar.xz архивы без привязки к локациям/провайдерам.~%~%")
  (format t "Использование:~%")
  (format t "  git-tree export [--days N] [--xz-dir PATH] [--verbose] [--help]~%~%")
  (format t "Опции:~%")
  (format t "  --days N       Архивировать только репозитории с коммитами не старее N дней (по умолчанию 7)~%")
  (format t "  --xz-dir PATH  Каталог для tar.xz архивов (по умолчанию ~~/.git-tree/xz)~%")
  (format t "  --verbose      Подробный вывод~%")
  (format t "  --help         Показать эту справку~%~%")
  (format t "Примеры:~%")
  (format t "  git-tree export~%")
  (format t "  git-tree export --days 1 --xz-dir ~~/.git-tree/xz~%")
  (format t "  git-tree export --verbose~%"))

(defun make-export-provider (xz-dir)
  "Создаёт временный provider для экспорта в указанный XZ-DIR."
  (make-instance 'cl-git-tree/loc:<provider>
                 :id "xz"
                 :description "Standalone tar.xz export"
                 :url-xz (cl-git-tree/fs:expand-home xz-dir)
                 :provider :local))

(defun export-repo (repo-dir days-filter verbose xz-dir)
  "Экспортирует один репозиторий в tar.xz. Возвращает 0 или 1."
  (let* ((ws (cl-git-tree/loc:make-workspace repo-dir))
         (repo-name (cl-git-tree/fs:repo-name repo-dir))
         (skip nil)
         (provider (make-export-provider xz-dir)))
    ;; Для согласованности с transport export пропускаем «грязные» репозитории.
    (unless (cl-git-tree/loc:repo-is-clean-p ws)
      (setf skip t)
      (when verbose
        (format t "~A ~A: пропущено (незакоммиченные изменения)~%"
                (cl-git-tree/loc:find-emo ws "warning")
                repo-name)))
    ;; Фильтрация по давности последнего коммита.
    (when (and (not skip) days-filter)
      (let ((days (cl-git-tree/loc:days-since-last-commit ws)))
        (when (or (null days) (> days days-filter))
          (setf skip t)
          (when verbose
            (format t "~A ~A: пропущено (коммит ~A дней назад)~%"
                    (cl-git-tree/loc:find-emo ws "warning")
                    repo-name
                    (or days "неизвестно"))))))
    (if skip
        0
        (cl-git-tree/loc:repo-transport-export ws provider
                                               :days-filter days-filter
                                               :verbose verbose))))

(defun cmd-export (&rest args)
  "CLI-команда: экспорт репозиториев в общий каталог tar.xz."
  (let* ((days-filter 7)
         (xz-dir "~/.git-tree/xz")
         (verbose (member "--verbose" args :test #'string=))
         (processed 0)
         (archived 0)
         (days-pos (position "--days" args :test #'string=))
         (xz-pos (position "--xz-dir" args :test #'string=)))

    (when (member "--help" args :test #'string=)
      (print-export-help)
      (return-from cmd-export))

    (when (and days-pos (< (1+ days-pos) (length args)))
      (setf days-filter (parse-integer (nth (1+ days-pos) args) :junk-allowed t)))

    (when (and xz-pos (< (1+ xz-pos) (length args)))
      (setf xz-dir (nth (1+ xz-pos) args)))

    (unless verbose
      (let ((ws (cl-git-tree/loc:make-workspace ".")))
        (format t "~A Архивирование репозиториев (--days ~A, --xz-dir ~A)...~%"
                (cl-git-tree/loc:find-emo ws "fs archive")
                days-filter
                (cl-git-tree/fs:expand-home xz-dir))))

    (flet ((export-one (repo-dir _args)
             (declare (ignore _args))
             (incf processed)
             (incf archived (export-repo repo-dir days-filter verbose xz-dir))))
      (cl-git-tree/fs:with-repo #'export-one args))

    (unless verbose
      (format t "~%=== Архивировано: ~A из ~A ===~%" archived processed))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "export" #'cmd-export "Экспорт tar.xz архивов в общий каталог"))
