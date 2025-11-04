;;;; ./src/commands/unclone.lisp

(defpackage :cl-git-tree/commands/unclone
  (:use :cl)
  (:export cmd-unclone))

(in-package :cl-git-tree/commands/unclone)

(defun unclone-repo (repo-dir args)
  "Удаляет bare-клон репозитория REPO-DIR из указанной LOCATION,
если он существует."
  (let* ((location  (cl-git-tree/loc:find-location (first args)))
         (repo-name (cl-git-tree/fs:repo-name repo-dir))
         (url (cl-git-tree/loc:<location>-url-git location))
         (target (merge-pathnames (format nil "~A.git/" repo-name)
                                  (uiop:ensure-directory-pathname url))))
    (cond
      ((uiop:directory-exists-p target)
       (uiop:delete-directory-tree target :validate t :if-does-not-exist :ignore)
       (format t "🗑 ~A: удалён клон ~A~%" repo-name target))
      (t
       (format t "⚠ ~A: клон в ~A не найден~%" repo-name target)))))

(defun unclone-repo (repo-dir args)
  "Удаляет bare-клон репозитория REPO-DIR из указанной LOCATION,
только если LOCATION прописана как remote."
  (let* ((location  (cl-git-tree/loc:find-location (first args)))
         (repo-name (cl-git-tree/fs:repo-name repo-dir))
         (loc-id    (cl-git-tree/loc:<location>-id location))
         (remotes   (cl-git-tree/git-utils:repo-remotes repo-dir)))
    (if (not (member loc-id remotes :test #'string=))
        (format t "⚠ ~A: локация ~A не является remote, пропускаем~%"
                repo-name loc-id)
        (let* ((url (cl-git-tree/loc:<location>-url-git location))
               (target (merge-pathnames (format nil "~A.git/" repo-name)
                                        (uiop:ensure-directory-pathname url))))
          (cond
            ((uiop:directory-exists-p target)
             (uiop:delete-directory-tree target :validate t :if-does-not-exist :ignore)
             (format t "🗑 ~A: удалён клон ~A~%" repo-name target))
            (t
             (format t "⚠ ~A: клон в ~A не найден~%" repo-name target)))))))


(defun cmd-unclone (&rest args)
  "CLI-команда: удалить bare-клоны всех локальных репозиториев из указанной локации.
Если LOCATION-NAME не указан или равен --help, выводится справка."
  (cond
    ;; показать справку
    ((or (null args)
         (string= (first args) "--help"))
     (format t "Удаляет bare-клоны всех локальных репозиториев из указанной локации.~%~%")
     (format t "Использование:~%  git-tree unclone LOCATION-NAME~%")
     (format t "Пример:~%  git-tree unclone gh~%"))

    ;; проверка существования локации
    (t
     (let* ((location-name (first args))
            (loc (cl-git-tree/loc:find-location location-name)))
       (if (null loc)
           (format t "⚠ Локация ~A не найдена в конфиге.~%" location-name)
           ;; запуск по дереву
           (cl-git-tree/fs:with-repo #'unclone-repo args))))))


(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "unclone" #'cmd-unclone "Удалить bare‑репозитории из локаций"))
