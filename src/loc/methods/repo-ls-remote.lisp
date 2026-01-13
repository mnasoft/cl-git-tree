;;;; ./src/loc/methods/repo-ls-remote.lisp

(in-package :cl-git-tree/loc)

(defmethod repo-ls-remote ((ws <workspace>) repository
                           &key branches tags refs upload-pack quiet
                                exit-code get-url sort symref patterns
                           &allow-other-keys)
  "Получить список ссылок из удалённого репозитория.
Аргументы:
  WS          → рабочее пространство (<workspace>)
  REPOSITORY  → репозиторий (URL или имя remote, например \"origin\")
Ключи:
  :branches   → T → показать только ветки (--branches)
  :tags       → T → показать только теги (--tags)
  :refs       → T → не показывать peeled tags (--refs)
  :upload-pack → путь к git-upload-pack на удалённом хосте
  :quiet      → T → не выводить URL удалённого репозитория (-q)
  :exit-code  → T → выйти с кодом 2 если ничего не найдено (--exit-code)
  :get-url    → T → показать URL и выйти (--get-url)
  :sort       → ключ для сортировки результатов (например \"refname\")
  :symref     → T → показать символьные ссылки (--symref)
  :patterns   → список паттернов для фильтрации ссылок"
  (let* ((args (list "ls-remote")))
    ;; Формируем аргументы команды
    (when branches
      (push "--branches" args))
    (when tags
      (push "--tags" args))
    (when refs
      (push "--refs" args))
    (when upload-pack
      (push (format nil "--upload-pack=~A" upload-pack) args))
    (when quiet
      (push "-q" args))
    (when exit-code
      (push "--exit-code" args))
    (when get-url
      (push "--get-url" args))
    (when sort
      (push (format nil "--sort=~A" sort) args))
    (when symref
      (push "--symref" args))
    ;; Добавляем репозиторий
    (when repository
      (push repository args))
    ;; Добавляем паттерны, если указаны
    (when patterns
      (if (listp patterns)
          (dolist (pattern patterns)
            (push pattern args))
          (push patterns args)))
    ;; Переворачиваем список аргументов
    (setf args (nreverse args))
    ;; Выполняем команду
    (multiple-value-bind (out err code)
        (apply #'cl-git-tree/git-utils:git-run (<workspace>-path ws) args)
      (cond
        ((or (zerop code) (and exit-code (= code 2)))
         (when get-url
           (format t "~A [~A] URL удалённого репозитория '~A':~%~A~%"
                   (find-emo ws "git remote -v")
                   (repo-name ws)
                   repository
                   (string-trim '(#\Newline #\Space) out)))
         (when (and (not get-url) (> (length out) 0))
           (unless quiet
             (format t "~A [~A] Ссылки в удалённом репозитории '~A':~%"
                     (find-emo ws "git remote -v")
                     (repo-name ws)
                     repository))
           (format t "~A" out))
         (values out code))
        (t
         (format t "~A Ошибка при получении списка ссылок из '~A' в ~A~%"
                 (find-emo ws "error")
                 repository
                 (repo-name ws))
         (when err
           (format t "  ~A~%" err))
         (values nil code))))))
