;;;; ./src/commands/unclone.lisp

(defpackage :cl-git-tree/commands/unclone
  (:use :cl)
  (:export cmd-unclone))

(in-package :cl-git-tree/commands/unclone)

(defun unclone-repo (repo-dir location)
  "Удаляет bare‑репозиторий из указанной LOCATION."
  (let* ((repo-name (cl-git-tree/fs:repo-name repo-dir))
         (base (uiop:ensure-directory-pathname (cl-git-tree/loc:<location>-url-git location)))
         (target (merge-pathnames (format nil "~A.git/" repo-name) base)))
    (cond
      ((probe-file target)
       (uiop:delete-directory-tree target :validate t :if-does-not-exist :ignore)
       (format t "🗑 ~A удалён из ~A~%" repo-name target))
      (t
       (format t "⚠ ~A: не найден в ~A~%" repo-name target)))))

(defun cmd-unclone (&optional location-name)
  "CLI-команда: удалить bare‑репозитории из указанной или всех локальных локаций."
  (let ((locations (if location-name
                       (let ((loc (cl-git-tree/loc:find-location location-name)))
                         (when loc (list loc)))
                       (remove-if-not #'cl-git-tree/loc:location-local-p
                                      (cl-git-tree/loc:all-locations)))))
    (if (null locations)
        (format t "⚠ Нет подходящих локаций для uncloning.~%")
        (cl-git-tree/fs:with-each-repo-simple
          (lambda (repo-dir)
            (dolist (loc locations)
              (unclone-repo repo-dir loc)))))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "unclone" #'cmd-unclone "Удалить bare‑репозитории из локаций"))
