;;;; ./src/commands/transport.lisp

(in-package :cl-git-tree/commands/transport)

(defun print-transport-help ()
  "Выводит справку по команде git-tree transport."
  (format t "Управление транспортом репозиториев через tar.xz архивы.~%~%")
  (format t "Использование:~%")
  (format t "  git-tree transport export [--days N] [--verbose]~%")
  (format t "  git-tree transport import [--keep-remote-dir] [--delete-archive] [--verbose]~%")
  (format t "  git-tree transport clean~%~%")
  (format t "Подкоманды:~%")
  (format t "  export               Создать tar.xz архивы из чистых репозиториев~%")
  (format t "  import               Импортировать архивы в bare-хранилища~%")
  (format t "  clean                Удалить tar.xz архивы из каталогов :url-xz всех провайдеров~%~%")
  (format t "Опции:~%")
  (format t "  --days N             Для export: архивировать только репозитории с коммитами не старее N дней (по умолчанию 30)~%")
  (format t "  --verbose            Для export/import: показать подробный вывод~%")
  (format t "  --keep-remote-dir    Для import: сохранить временный каталог remote после pull (по умолчанию удаляется)~%")
  (format t "  --delete-archive     Для import: удалить архив после успешного импорта~%")
  (format t "  --help               Показать эту справку~%~%")
  (format t "Примечание:~%")
  (format t "  Архивы создаются и импортируются для каждого локального провайдера в папки :url-xz и :url-git.~%")
  (format t "  Если :url-xz = NIL, операции для этого провайдера пропускаются.~%~%")
  (format t "Примеры:~%")
  (format t "  git-tree transport export               # краткий вывод~%")
  (format t "  git-tree transport export --verbose     # подробный вывод~%")
  (format t "  git-tree transport export --days 7~%")
  (format t "  git-tree transport import --keep-remote-dir~%")
  (format t "  git-tree transport import --delete-archive~%")
  (format t "  git-tree transport import --verbose~%")
  (format t "  git-tree transport clean~%"))


(defun cmd-transport (&rest args)
  "CLI-команда: архивирует чистые репозитории в tar.xz или очищает каталоги с архивами.
  
  Опции:
    --provider PROVIDER  - фильтр по провайдеру (например, :local, :github)
    --days N             - архивировать только репозитории, обновлённые не позднее N дней назад (по умолчанию 30)
    --output PATH        - путь для архивов/очистки (по умолчанию ~/.git-tree/xz/)
    --help               - показать эту справку"
  (cond
    ((or (null args) (member "--help" args :test #'string=))
     (print-transport-help))
    ((and args (string= (first args) "clean"))
     (transport-clean))
    ((and args (string= (first args) "import"))
     (transport-import (rest args)))
    ((and args (string= (first args) "export"))
     (transport-export (rest args)))
    (t
     (let ((ws (cl-git-tree/loc:make-workspace ".")))
       (format t "~A Неизвестная подкоманда. Используйте: export, import или clean.~%" (cl-git-tree/loc:find-emo ws "error"))
       (format t "Справка: git-tree transport --help~%")))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "transport" #'cmd-transport "Управление транспортом репозиториев через tar.xz"))
