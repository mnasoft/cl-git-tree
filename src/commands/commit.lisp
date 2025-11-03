;;;; ./src/commands/commit.lisp

(defpackage :cl-git-tree/commands/commit
  (:use :cl)
  (:export cmd-commit
           commit-repo))

(in-package :cl-git-tree/commands/commit)

(defun commit-repo (repo-dir args)
  "Выполняет git commit -am MESSAGE в одном репозитории REPO-DIR.
Если MESSAGE отсутствует, используется текущая дата."
  (let* ((message (if args
                      (format nil "~{~A~^ ~}" args) ; склеиваем все слова
                      (multiple-value-bind (sec min hour day mon year dow dst tz)
                          (get-decoded-time)
                        (declare (ignore dow dst tz))
                        (format nil "Commit ~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                                year mon day hour min sec)))))

    (format t "→ ~A: git commit -am ~S~%" repo-dir message)
    (multiple-value-bind (_out err code)
        (cl-git-tree/git-utils:git-run repo-dir "commit" "-am" message)
      (declare (ignore _out))
      (if (zerop code)
          (format t "✔ ~A: commit ~A~%" repo-dir message)
          (format t "❌ ~A: ошибка commit:~%~A" repo-dir err)))))


(defun cmd-commit (&rest args)
  "CLI-команда: выполнить git commit -am MESSAGE во всех репозиториях.
Если MESSAGE отсутствует, используется текущая дата."
  (cond
    ;; Показать справку
    ((member "--help" args :test #'string=)
     (format t "Выполняет git commit -am MESSAGE во всех git-репозиториях.~%~%")
     (format t "Использование:~%  git-tree commit [MESSAGE...]~%")
     (format t "Если MESSAGE не указан, используется текущая дата.~%")
     (format t "Пример:~%  git-tree commit \"Fix bug in parser\"~%"))
    ;; Запуск по дереву
    (t
     (cl-git-tree/fs:with-repo #'commit-repo args))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "commit" #'cmd-commit "Сделать git commit -am MESSAGE во всех репозиториях"))

