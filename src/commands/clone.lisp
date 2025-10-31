;;;; ./src/commands/clone.lisp

(defpackage :cl-git-tree/commands/clone
  (:use :cl)
  (:import-from cl-git-tree/git-utils
                git-run)
  (:import-from cl-git-tree
                find-location
                all-locations
                location-url-git
                location-local-p)
  (:import-from cl-git-tree/fs
                repo-name
                with-each-repo-simple)
  (:export run))

(in-package :cl-git-tree/commands/clone)

(defun clone-repo (repo-dir location)
  (let* ((repo-name (repo-name repo-dir))
         (url (location-url-git location))
         (target (merge-pathnames (format nil "~A.git" repo-name)
                 (uiop:ensure-directory-pathname url))))
    (cond
      ;; если уже существует — пропускаем
      ((probe-file target)
       (format t "⚠ ~A: уже существует ~A~%" repo-name target))
      (t
       (ensure-directories-exist target)
       (multiple-value-bind (out err code)
           (git-run repo-dir "clone" "--bare" "." (namestring target))
         (declare (ignore out))
         (if (zerop code)
             (format t "✔ ~A → ~A~%" repo-name target)
             (format t "❌ ~A: clone failed~%~A~%" repo-name err)))))))

(defun run (&optional location-name)
  (let ((locations (if location-name
                       (let ((loc (find-location location-name)))
                         (if loc (list loc) nil))
                       (remove-if-not #'location-local-p (all-locations)))))
    (if (null locations)
        (format t "⚠ Нет подходящих локаций для клонирования.~%")
        (with-each-repo-simple
          (lambda (repo-dir)
            (dolist (loc locations)
              (clone-repo repo-dir loc)))))))

(push (cons "clone" #'run) cl-git-tree:*commands*)

