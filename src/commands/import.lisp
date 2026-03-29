;;;; ./src/commands/import.lisp

(defpackage :cl-git-tree/commands/import
  (:use :cl)
  (:export cmd-import
           import-repo))

(in-package :cl-git-tree/commands/import)

(defun print-import-help ()
  "Справка по git-tree import."
  (format t "Импорт изменений из tar.xz архивов без привязки к локациям/провайдерам.~%~%")
  (format t "Использование:~%")
  (format t "  git-tree import [--xz-dir PATH] [--keep-remote-dir] [--delete-archive] [--verbose] [--help]~%~%")
  (format t "Опции:~%")
  (format t "  --xz-dir PATH       Каталог tar.xz архивов (по умолчанию ~~/.git-tree/xz)~%")
  (format t "  --keep-remote-dir   Сохранить распакованный каталог после импорта~%")
  (format t "  --delete-archive    Удалить архив после успешного импорта~%")
  (format t "  --verbose           Подробный вывод~%")
  (format t "  --help              Показать эту справку~%~%")
  (format t "Примеры:~%")
  (format t "  git-tree import~%")
  (format t "  git-tree import --xz-dir ~~/.git-tree/xz --delete-archive~%")
  (format t "  git-tree import --keep-remote-dir --verbose~%"))

(defun make-import-provider (xz-dir)
  "Создаёт временный provider для импорта из указанного XZ-DIR."
  (make-instance 'cl-git-tree/loc:<provider>
                 :id "xz"
                 :description "Standalone tar.xz import"
                 :url-xz (cl-git-tree/fs:expand-home xz-dir)
                 :provider :local))

(defun import-repo (repo-dir verbose xz-dir &key keep-remote-dir delete-archive)
  "Импортирует изменения для одного репозитория.
Возвращает два значения: success-count и fail-count." 
  (let* ((ws (cl-git-tree/loc:make-workspace repo-dir))
         (provider (make-import-provider xz-dir)))
    (multiple-value-bind (success attempted)
        (cl-git-tree/loc:repo-transport-import
         ws
         provider
         :verbose verbose
         :keep-remote-dir keep-remote-dir
         :delete-archive delete-archive)
      (if attempted
          (if success
              (values 1 0)
              (values 0 1))
          (values 0 0)))))

(defun cmd-import (&rest args)
  "CLI-команда: импорт tar.xz архивов из общего каталога."
  (let* ((xz-dir "~/.git-tree/xz")
         (verbose (member "--verbose" args :test #'string=))
         (keep-remote-dir (member "--keep-remote-dir" args :test #'string=))
         (delete-archive (member "--delete-archive" args :test #'string=))
         (processed 0)
         (ff-success-total 0)
         (ff-failed-total 0)
         (xz-pos (position "--xz-dir" args :test #'string=)))

    (when (member "--help" args :test #'string=)
      (print-import-help)
      (return-from cmd-import))

    (when (and xz-pos (< (1+ xz-pos) (length args)))
      (setf xz-dir (nth (1+ xz-pos) args)))

    (unless verbose
      (format t "⬇ Импорт изменений из архивов (--xz-dir ~A)...~%"
              (cl-git-tree/fs:expand-home xz-dir)))

    (flet ((import-one (repo-dir _args)
             (declare (ignore _args))
             (incf processed)
             (multiple-value-bind (succ fail)
                 (import-repo repo-dir verbose xz-dir
                              :keep-remote-dir keep-remote-dir
                              :delete-archive delete-archive)
               (incf ff-success-total succ)
               (incf ff-failed-total fail))))
      (cl-git-tree/fs:with-repo #'import-one args))

    (unless verbose
      (format t "~%=== Просмотрено каталогов: ~A; FF-успехов: ~A; FF-ошибок: ~A ===~%"
              processed ff-success-total ff-failed-total))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "import" #'cmd-import "Импорт tar.xz архивов из общего каталога"))
