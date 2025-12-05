;;;; ./src/commands/remote-delete.lisp

(defpackage :cl-git-tree/commands/remote-delete
  (:use :cl)
  (:export cmd-remote-delete))

(in-package :cl-git-tree/commands/remote-delete)

(defun remote-delete-workspace (repo-dir args)
  "Удаляет репозиторий через метод remote-delete для указанной локации."
  (let* ((loc-key (first args))
         (loc (cl-git-tree/loc:find-location loc-key)))
    (unless loc
      (format t "⚠ Локация ~A не найдена~%" loc-key)
      (return-from remote-delete-workspace))
    (let* ((ws (cl-git-tree/loc:make-workspace repo-dir)))
      (cl-git-tree/loc:remote-delete ws loc :yes t :remote-only nil))))

(defun cmd-remote-delete (&rest args)
  "CLI-команда: удалить репозитории для всех локальных git-каталогов через метод remote-delete.
Если LOCATION-NAME не указан или равен --help, выводится справка."
  (cond
    ;; показать справку
    ((or (null args)
         (string= (first args) "--help"))
     (format t "Удаляет репозитории для всех локальных git-каталогов из указанной локации.~%~%")
     (format t "Использование:~%  git-tree unclone LOCATION-NAME~%")
     (format t "Пример:~%  git-tree unclone gh~%"))

    ;; проверка существования локации
    (t
     (let* ((location-name (first args))
            (loc (cl-git-tree/loc:find-location location-name)))
       (if (null loc)
           (format t "⚠ Локация ~A не найдена в конфиге.~%" location-name)
           ;; запуск по дереву
           (cl-git-tree/fs:with-repo #'remote-delete-workspace args))))))

;; Регистрация перемещена в remote.lisp (используется как подкоманда 'remote delete')
