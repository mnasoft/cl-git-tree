;;;; ./src/commands/remote-add.lisp

(defpackage :cl-git-tree/commands/remote-add
  (:use :cl)
  (:export add-remote-to-repo
           cmd-remote-add))

(in-package :cl-git-tree/commands/remote-add)

(defun add-remote-to-repo (repo-dir args) 
  "Добавляет remote LOC-KEY в один репозиторий."
  (let* ((loc-key (first args))
         (loc (gethash loc-key cl-git-tree/loc:*locations*))
         (base-url (and loc (cl-git-tree/loc:<location>-url-git loc))))
    (if (and loc-key loc base-url)
        (let* ((repo-name (cl-git-tree/fs:repo-name repo-dir))
               (remote-url (format nil "~A/~A.git" base-url repo-name)))
          (multiple-value-bind (out _err _code)
              (cl-git-tree/git-utils:git-run repo-dir "remote")
            (declare (ignore _err _code))
            (if (search loc-key out :test #'string=)
                (format t "⚠️  В ~A remote ~A уже существует~%" repo-dir loc-key)
                (progn
                  (format t "→ ~A: git remote add ~A ~A~%"
                          repo-dir loc-key remote-url)
                  (cl-git-tree/git-utils:git-run repo-dir "remote" "add" loc-key remote-url)))))
        (format t "⚠️  Неизвестная локация: ~A~%" loc-key))))


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

;; Регистрация перемещена в remote.lisp (используется как подкоманда 'remote add')
