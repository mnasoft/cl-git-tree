;;;; ./src/commands/commit.lisp
 
(defpackage :cl-git-tree/commands/commit
  (:use :cl
        :cl-git-tree/config)
  (:export cmd-commit
           commit-repo))

(in-package :cl-git-tree/commands/commit)

(defun commit-repo (repo-dir args)
  "Выполняет git commit в одном репозитории REPO-DIR.
Поддерживает ключи:
  -a        → добавить все изменённые файлы
  -m <msg>  → сообщение коммита
  -am <msg> → комбинация -a и -m
  --amend   → изменить последний коммит
Если MESSAGE отсутствует, используется текущая дата/время."
  (let ((all nil)
        (amend nil)
        (message nil))
    ;; Разбор аргументов
    (loop for i from 0 below (length args) do
      (let ((arg (nth i args)))
        (cond
          ;; -a
          ((string= arg "-a")
           (setf all t))
          ;; --amend
          ((string= arg "--amend")
           (setf amend t))
          ;; -m <msg>
          ((string= arg "-m")
           (when (< (+ i 1) (length args))
             (setf message (nth (+ i 1) args)))
           (return))
          ;; -am <msg>
          ((string= arg "-am")
           (setf all t)
           (when (< (+ i 1) (length args))
             (setf message (nth (+ i 1) args)))
           (return)))))

    ;; Если сообщение не указано, используем текущую дату и время
    (unless message
      (multiple-value-bind (sec min hour day month year)
          (decode-universal-time (get-universal-time))
        (setf message (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                              year month day hour min sec))))

    ;; Вызов метода repo-commit
    (let ((ws (make-instance 'cl-git-tree/loc:<workspace> :path repo-dir)))
      (let ((result (cl-git-tree/loc:repo-commit ws :all all :amend amend :message message)))
        (format t "~A ~A: ~A~%" (cl-git-tree/loc:find-emo ws "success") repo-dir result)))))

(defun cmd-commit (&rest args)
  "CLI-команда: выполнить git commit во всех репозиториях.
Поддерживает ключи:
  -a        → добавить все изменённые файлы
  -m <msg>  → сообщение коммита
  -am <msg> → комбинация -a и -m
  --amend   → изменить последний коммит
Если MESSAGE отсутствует, используется текущая дата/время."
  (cond
    ;; Показать справку
    ((member "--help" args :test #'string=)
     (format t "Выполняет git commit во всех git-репозиториях.~%~%")
     (format t "Использование:~%")
     (format t "  git-tree commit [-a] [-m <msg>] [-am <msg>] [--amend]~%~%")
     (format t "Если MESSAGE не указан, используется текущая дата/время.~%~%")
     (format t "Примеры:~%")
     (format t "  git-tree commit -a -m \"Fix bug in parser\"~%")
     (format t "  git-tree commit -am \"Initial commit\"~%")
     (format t "  git-tree commit --amend~%")
     (format t "  git-tree commit ;; сообщение = текущая дата/время~%"))
    ;; Запуск по дереву
    (t
     (cl-git-tree/fs:with-repo #'commit-repo args))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "commit" #'cmd-commit "Сделать git commit -am MESSAGE во всех репозиториях"))
