;;;; ./src/commands/remote.lisp

(defpackage :cl-git-tree/commands/remote
  (:use :cl)
  (:export cmd-remote))

(in-package :cl-git-tree/commands/remote)

(defun cmd-remote (&rest args)
  "Главный обработчик команды 'remote' с подкомандами: add, remove, readd, create, delete, list.

Использование:
  git-tree remote add LOC-KEY
  git-tree remote remove LOC-KEY
  git-tree remote readd LOC-KEY
  git-tree remote create LOC-KEY [--private]
  git-tree remote list
  git-tree remote --help"
  (cond
    ;; Показать справку
    ((or (null args)
         (member "--help" args :test #'string=))
     (format t "Управление remote в git-репозиториях.~%~%")
     (format t "Подкоманды:~%")
     (format t "  git-tree remote list             - Показать все доступные локации~%")
     (format t "  git-tree remote add LOC-KEY      - Добавить remote во все репозитории~%")
    (format t "  git-tree remote remove LOC-KEY   - Удалить remote из всех репозиториев~%")
    (format t "  git-tree remote readd LOC-KEY    - Заново добавить remote во все репозитории~%")
    (format t "  git-tree remote create LOC-KEY [--private] - Создать репозиторий на провайдере для каждого локального git~%")
    (format t "  git-tree remote delete LOC-KEY   - Удалить репозиторий на провайдере для каждого локального git~%")
     (format t "~%Пример:~%  git-tree remote add gh~%"))
    
    ;; Подкоманда 'list'
    ((string= (first args) "list")
     (cl-git-tree/loc:print-locations))
    
    ;; Подкоманда 'add'
    ((string= (first args) "add")
     (apply #'cl-git-tree/commands/remote-add:cmd-remote-add (rest args)))
    
    ;; Подкоманда 'remove'
    ((string= (first args) "remove")
     (apply #'cl-git-tree/commands/remote-remove:cmd-remote-remove (rest args)))
    
    ;; Подкоманда 'readd'
    ((string= (first args) "readd")
     (apply #'cl-git-tree/commands/remote-readd:cmd-remote-readd (rest args)))

    ;; Подкоманда 'delete' — обёртка над remote-delete
    ((string= (first args) "delete")
     (apply #'cl-git-tree/commands/remote-delete:cmd-remote-delete (rest args)))

    ;; Подкоманда 'create' — обёртка над remote-create
    ((string= (first args) "create")
     (apply #'cl-git-tree/commands/remote-create:cmd-remote-create (rest args)))
    
    ;; Неизвестная подкоманда
    (t
     (format t "Неизвестная подкоманда: ~A~%" (first args))
     (format t "Используйте 'git-tree remote --help' для справки~%"))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "remote" #'cmd-remote "Управление remote в репозиториях (list/add/remove/readd/create/delete)"))
