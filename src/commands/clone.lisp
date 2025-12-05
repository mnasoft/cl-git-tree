;;;; ./src/commands/clone.lisp

(defpackage :cl-git-tree/commands/clone
  (:use :cl)
  (:export cmd-clone))

(in-package :cl-git-tree/commands/clone)

#+nil
(defun clone-repo (repo-dir args)
  "Клонирует репозиторий REPO-DIR в указанную LOCATION как bare-репозиторий,
но только если LOCATION уже прописана как remote в этом репозитории."
  (let* ((location  (cl-git-tree/loc:find-location (first args)))
         (repo-name (cl-git-tree/fs:repo-name repo-dir))
         (loc-id    (cl-git-tree/loc:<location>-id location))
         (remotes   (cl-git-tree/git-utils:repo-remotes repo-dir)))
    (if (not (member loc-id remotes :test #'string=))
        (format t "⚠ ~A: локация ~A не добавлена как remote, пропускаем~%"
                repo-name loc-id)
        (let* ((url (cl-git-tree/loc:<location>-url-git location))
               (target (merge-pathnames (format nil "~A.git" repo-name)
                                        (uiop:ensure-directory-pathname url))))
          (cond
            ;; если уже существует — пропускаем
            ((probe-file target)
             (format t "⚠ ~A: уже существует ~A~%" repo-name target))
            (t
             (ensure-directories-exist target)
             (multiple-value-bind (out err code)
                 (cl-git-tree/git-utils:git-run repo-dir "clone" "--bare" "." (namestring target))
               (declare (ignore out))
               (if (zerop code)
                   (format t "✔ ~A → ~A~%" repo-name target)
                   (format t "❌ ~A: clone failed~%~A~%" repo-name err)))))))))

(defun clone-repo (repo-dir args)
  "Клонирует репозиторий REPO-DIR в указанную LOCATION как bare-репозиторий,
но только если LOCATION уже прописана как remote в этом репозитории."
  (let* ((loc-key (first args))
         (pr (cl-git-tree/loc:find-location loc-key))
         (ws (cl-git-tree/loc:make-workspace repo-dir)))
    (cl-git-tree/loc:repo-clone ws pr)))

(defun cmd-remote-add (&rest args)
  "CLI-команда: добавить remote LOC-KEY во все git-репозитории.

Пример использования:
  git-tree remote-add gh"
  (cond
    ;; Показать справку
    ((or (null args)
         (member "--help" args :test #'string=))
     (format t "Добавляет remote во все git-репозитории.~%~%")
     (format t "Использование:~%  git-tree remote-add LOC-KEY~%")
     (format t "Пример:~%  git-tree remote-add gh~%"))
    ;; Запуск по дереву
    (t
     (cl-git-tree/fs:with-repo #'add-remote-to-repo args))))

(defun cmd-clone (&rest args)
  "CLI-команда: клонировать все локальные репозитории в указанную локацию.
Если LOCATION-NAME не указан или равен --help, выводится справка."
  (cond
    ;; показать справку
    ((or (null args)
         (string= (first args) "--help"))
     (format t "Клонирует все локальные репозитории в указанную локацию,~%")
     (format t "если она прописана в конфиге и добавлена как remote.~%~%")
     (format t "Использование:~%  git-tree clone LOCATION-NAME~%")
     (format t "Пример:~%  git-tree clone gh~%"))

    ;; проверка существования локации
    (t
     (let* ((location-name (first args))
            (loc (cl-git-tree/loc:find-location location-name)))
       (if (null loc)
           (format t "⚠ Локация ~A не найдена в конфиге.~%" location-name)
           ;; запуск по дереву
           (cl-git-tree/fs:with-repo #'clone-repo args))))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "clone" #'cmd-clone "Клонировать все локальные репозитории"))
