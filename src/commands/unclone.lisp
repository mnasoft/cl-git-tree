;;;; ./src/commands/unclone.lisp

(defpackage :cl-git-tree/commands/unclone
  (:use :cl)
  (:import-from cl-git-tree
                find-location
                all-locations
                location-url-git
                location-local-p)
  (:import-from cl-git-tree/fs
                repo-name
                with-each-repo-simple)
  (:export run))

(in-package :cl-git-tree/commands/unclone)

(defun unclone-repo (repo-dir location)
  (let* ((repo-name (repo-name repo-dir))
         (base (uiop:ensure-directory-pathname (location-url-git location)))
         (target (merge-pathnames (format nil "~A.git/" repo-name) base)))
    (cond
      ((probe-file target)
       (uiop:delete-directory-tree target :validate t :if-does-not-exist :ignore)
       (format t "🗑 ~A удалён из ~A~%" repo-name target))
      (t
       (format t "⚠ ~A: не найден в ~A~%" repo-name target)))))

(defun run (&optional location-name)
  "Если указана локация — удаляем из неё.
   Если нет — пробуем все локальные локации."
  (let ((locations (if location-name
                       (let ((loc (find-location location-name)))
                         (if loc (list loc) nil))
                       (remove-if-not #'location-local-p (all-locations)))))
    (if (null locations)
        (format t "⚠ Нет подходящих локаций для uncloning.~%")
        (with-each-repo-simple
          (lambda (repo-dir)
            (dolist (loc locations)
              (unclone-repo repo-dir loc)))))))

(push (cons "unclone" #'run) cl-git-tree:*commands*)
