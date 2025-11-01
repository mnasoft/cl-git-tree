;;;; ./src/cli.lisp

(defpackage :cl-git-tree/cli
  (:use :cl)
  (:documentation
   "Подсистема CLI: точка входа и справка по доступным командам git-tree.")
  (:export main))

(in-package :cl-git-tree/cli)

(defun print-help ()
  "Выводит справку по доступным командам git-tree."
  (cl-git-tree/dispatch:show-version)
  (format t "Usage: git-tree <command> [args...]~%~%")
  (format t "Доступные команды:~%")
  (format t "  add <files...>                - добавить файлы в индекс~%")
  (format t "  commit [msg]                  - создать коммит~%")
  (format t "  pull                          - выполнить git pull во всех репозиториях~%")
  (format t "  push                          - выполнить git push во всех репозиториях~%")
  (format t "  all                           - выполнить pull → add → commit → push~%")
  (format t "  clone [loc]                   - клонировать репозитории по локации~%")
  (format t "  unclone [loc]                 - удалить локальные bare‑клоны~%")
  (format t "  remote-add <loc>              - добавить git remote~%")
  (format t "  remote-remove <loc>           - удалить git remote~%")
  (format t "  remote-readd <loc>            - пересоздать git remote~%")
  (format t "  remake-xz <loc>               - пересоздать архив .xz для локации~%")
  (format t "  info                          - показать список локаций~%")
  (format t "  aliases [list]                - показать или настроить алиасы~%")
  (format t "  version                       - показать версию~%")
  (format t "  help                          - эта справка~%"))

(defun main (argv)
  "CLI‑точка входа. ARGV — список аргументов командной строки."
  (cl-git-tree:load-config)
  (let ((args (rest argv))) ; первый элемент — имя скрипта
    (cond
      ((or (null args)
           (string= (first args) "--help")
           (string= (first args) "help"))
       (print-help))
      ((string= (first args) "version")
       (cl-git-tree/dispatch:show-version))
      (t
       (cl-git-tree/dispatch:dispatch-command (first args) (rest args))))))
