;;;; ./src/loc/methods/repo-fetch.lisp

(in-package :cl-git-tree/loc)

(defmethod repo-fetch ((ws <workspace>)
                       &key remote all multiple prune prune-tags tags depth
                            atomic no-recurse-submodules dry-run quiet
                       &allow-other-keys)
  "Загрузить объекты и ссылки из удалённого репозитория.
Аргументы:
  WS          → рабочее пространство (<workspace>)
Ключи:
  :remote     → имя удалённого репозитория (по умолчанию \"origin\")
  :all        → T → загрузить из всех удалённых репозиториев (--all)
  :multiple   → T → включить несколько репозиториев (--multiple)
  :prune      → T → удалить удалённые ссылки (--prune)
  :prune-tags → T → удалить удалённые теги (--prune-tags)
  :tags       → T → загрузить все теги (--tags)
  :depth      → глубина для shallow clone (--depth)
  :atomic     → T → использовать atomic транзакции (--atomic)
  :no-recurse-submodules → T → не загружать подмодули (--no-recurse-submodules)
  :dry-run    → T → пробный запуск (--dry-run)
  :quiet      → T → подавить вывод (-q)"
  (let* ((args (list "fetch")))
    ;; Формируем аргументы команды
    (when quiet
      (push "-q" args))
    (when all
      (push "--all" args))
    (when multiple
      (push "--multiple" args))
    (when prune
      (push "--prune" args))
    (when prune-tags
      (push "--prune-tags" args))
    (when tags
      (push "--tags" args))
    (when depth
      (push (format nil "--depth=~A" depth) args))
    (when atomic
      (push "--atomic" args))
    (when no-recurse-submodules
      (push "--no-recurse-submodules" args))
    (when dry-run
      (push "--dry-run" args))
    ;; Добавляем remote, если указан и не используется --all
    (when (and remote (not all))
      (push remote args))
    ;; Переворачиваем список аргументов
    (setf args (nreverse args))
    ;; Выполняем команду
    (multiple-value-bind (out err code)
        (apply #'cl-git-tree/git-utils:git-run (<workspace>-path ws) args)
      (cond
        ((zerop code)
         (format t "~A [~A] Fetch успешно завершен~@[ (~A)~]~%"
                 (find-emo ws "git fetch")
                 (repo-name ws)
                 remote)
         (when (and out (> (length out) 0) (not quiet))
           (format t "~A~%" out))
         t)
        (t
         (format t "~A Ошибка при выполнении git fetch в ~A~%"
                 (find-emo ws "error")
                 (repo-name ws))
         (when err
           (format t "  ~A~%" err))
         nil)))))
