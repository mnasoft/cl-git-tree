;;;; ./src/commands/transport.lisp

(in-package :cl-git-tree/commands/transport)

(defun print-transport-help ()
  "Выводит справку по команде git-tree transport."
  (format t "Управление транспортом репозиториев через tar.xz архивы.~%~%")
  (format t "Использование:~%")
  (format t "  git-tree transport export [--days N] [--verbose]~%")
  (format t "  git-tree transport import [--keep-remote-dir] [--delete-archive] [--verbose]~%")
  (format t "  git-tree transport clone <archive.tar.xz> <location>~%")
  (format t "  git-tree transport list <location>~%")
  (format t "  git-tree transport list-git <location>~%")
  (format t "  git-tree transport clean~%~%")
  (format t "Подкоманды:~%")
  (format t "  export               Создать tar.xz архивы из чистых репозиториев~%")
  (format t "  import               Импортировать архивы в bare-хранилища~%")
  (format t "  clone                Клонировать репозиторий из архива с созданием remote~%")
  (format t "  list                 Вывести список tar.xz архивов для локации~%")
  (format t "  list-git             Вывести список git-репозиториев для локации~%")
  (format t "  clean                Удалить tar.xz архивы из каталогов :url-xz всех провайдеров~%~%")
  (format t "Опции кратко:~%")
  (format t "  export   --days N | --verbose | --help~%")
  (format t "  import   --keep-remote-dir | --delete-archive | --verbose | --help~%")
  (format t "  clone    <archive.tar.xz> <location> | --help~%")
  (format t "  list     <location> | --help~%")
  (format t "  list-git <location> | --help~%")
  (format t "  clean    --help~%~%")
  (format t "Для подробной справки используйте: git-tree transport <subcmd> --help~%"))


(defun cmd-transport (&rest args)
  "CLI-команда: архивирует чистые репозитории в tar.xz или очищает каталоги с архивами.
  
  Опции:
    --provider PROVIDER  - фильтр по провайдеру (например, :local, :github)
    --days N             - архивировать только репозитории, обновлённые не позднее N дней назад (по умолчанию 30)
    --output PATH        - путь для архивов/очистки (по умолчанию ~/.git-tree/xz/)
    --help               - показать эту справку"
  (let ((subcmd (first args))
        (rest-args (rest args)))
    (cond
      ((and subcmd (string= subcmd "clean"))
       (transport-clean rest-args))
      ((and subcmd (string= subcmd "import"))
       (transport-import rest-args))
      ((and subcmd (string= subcmd "export"))
       (transport-export rest-args))
      ((and subcmd (string= subcmd "clone"))
       (transport-clone rest-args))
      ((and subcmd (string= subcmd "list"))
       (transport-list rest-args))
      ((and subcmd (string= subcmd "list-git"))
       (transport-list-git rest-args))
      ((or (null args) (member "--help" args :test #'string=))
       (print-transport-help))
      (t
       (let ((ws (cl-git-tree/loc:make-workspace ".")))
         (format t "~A Неизвестная подкоманда. Используйте: export, import, clone, list, list-git или clean.~%" (cl-git-tree/loc:find-emo ws "error"))
         (format t "Справка: git-tree transport --help~%"))))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "transport" #'cmd-transport "Управление транспортом репозиториев через tar.xz"))
