;;;; ./src/commands/remote-remove.lisp

(defpackage :cl-git-tree/commands/remote-remove
  (:use :cl)
  (:export cmd-remote-remove
           remove-remote-from-repo))

(in-package :cl-git-tree/commands/remote-remove)

#+nil
(defun remove-remote-from-repo (repo-dir loc-key base-url)
  "Удаляет remote LOC-KEY из одного репозитория."
  (declare (ignore base-url))
  (multiple-value-bind (out _err _code)
      (cl-git-tree/git-utils:git-run repo-dir "remote")
    (declare (ignore _err _code))
    (if (search loc-key out :test #'string=)
        (progn
          (format t "→ ~A: git remote remove ~A~%" repo-dir loc-key)
          (cl-git-tree/git-utils:git-run repo-dir "remote" "remove" loc-key))
        (format t "⚠️  В ~A remote ~A не найден~%" repo-dir loc-key))))

(defun remove-remote-from-repo (repo-dir args)
  "Удаляет remote LOC-KEY из одного репозитория REPO-DIR.

LOC-KEY передаётся как первый элемент ARGS."
  (let ((loc-key (first args)))
    (multiple-value-bind (out _err _code)
        (cl-git-tree/git-utils:git-run repo-dir "remote")
      (declare (ignore _err _code))
      (if (search loc-key out :test #'string=)
          (progn
            (format t "→ ~A: git remote remove ~A~%" repo-dir loc-key)
            (cl-git-tree/git-utils:git-run repo-dir "remote" "remove" loc-key))
          (format t "⚠️  В ~A remote ~A не найден~%" repo-dir loc-key)))))

#+nil
(defun cmd-remote-remove (loc-key &rest _args)
  "CLI-команда: найти все git-репозитории и удалить remote LOC-KEY."
  (cl-git-tree/fs:with-each-repo loc-key #'remove-remote-from-repo))

(defun cmd-remote-remove (&rest args)
  "CLI-команда: удалить remote LOC-KEY из одного репозитория.

Пример использования:
  git-tree remote-remove gh
  git-tree remote-remove gh --verbose"
  (cond
    ;; Показать справку
    ((or (null args)
         (member "--help" args :test #'string=))
     (format t "Удаляет remote из одного git-репозитория.~%~%")
     (format t "Использование:~%  git-tree remote-remove LOC-KEY [--verbose]~%")
     (format t "Пример:~%  git-tree remote-remove gh~%"))
    ;; Запуск по дереву
    (t
     (cl-git-tree/fs:with-repo #'remove-remote-from-repo args))))

;; Регистрация перемещена в remote.lisp (используется как подкоманда 'remote remove')



