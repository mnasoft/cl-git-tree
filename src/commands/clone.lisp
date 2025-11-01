;;;; ./src/commands/clone.lisp

(defpackage :cl-git-tree/commands/clone
  (:use :cl)
  (:export cmd-clone))

(in-package :cl-git-tree/commands/clone)

(defun clone-repo (repo-dir location)
  "Клонирует репозиторий REPO-DIR в указанную LOCATION как bare-репозиторий."
  (let* ((repo-name (cl-git-tree/fs:repo-name repo-dir))
         (url (cl-git-tree/loc:<location>-url-git location))
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
             (format t "❌ ~A: clone failed~%~A~%" repo-name err)))))))

(defun cmd-clone (&optional location-name)
  "CLI-команда: клонировать все локальные репозитории в указанные локации.
Если LOCATION-NAME задано, используется только эта локация."
  (let ((locations (if location-name
                       (let ((loc (cl-git-tree/loc:find-location location-name)))
                         (when loc (list loc)))
                       (remove-if-not #'cl-git-tree/loc:location-local-p
                                      (cl-git-tree/loc:all-locations)))))
    (if (null locations)
        (format t "⚠ Нет подходящих локаций для клонирования.~%")
        (cl-git-tree/fs:with-each-repo-simple
          (lambda (repo-dir)
            (dolist (loc locations)
              (clone-repo repo-dir loc)))))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "clone" #'cmd-clone "Клонировать все локальные репозитории"))
