;;;; ./src/loc/methods/repo-checkout.lisp

(in-package :cl-git-tree/loc)

(defmethod repo-checkout ((ws <workspace>) target
                          &key quiet force merge detach
                               create create-force orphan start-point
                               pathspec ours theirs conflict patch
                          &allow-other-keys)
  "Выполнить git checkout в рабочем пространстве WS.
Аргументы:
  WS          → рабочее пространство (<workspace>)
  TARGET      → имя ветки, коммита или tree-ish для checkout
Ключи:
  :quiet      → T → подавить вывод (-q)
  :force      → T → принудительный checkout (-f)
  :merge      → T → выполнить трёхсторонее слияние (-m)
  :detach     → T → отсоединённое состояние HEAD (--detach)
  :create     → T → создать новую ветку (-b)
  :create-force → T → создать или пересоздать ветку (-B)
  :orphan     → T → создать ветку-сироту без истории (--orphan)
  :start-point → начальная точка для новой ветки (коммит/ветка/тег)
  :pathspec   → список путей для восстановления файлов
  :ours       → T → разрешить конфликты в пользу нашей версии (--ours)
  :theirs     → T → разрешить конфликты в пользу их версии (--theirs)
  :conflict   → стиль маркеров конфликтов (\"merge\", \"diff3\", \"zdiff3\")
  :patch      → T → интерактивный режим выбора изменений (-p)"
  (let* ((args (list "checkout")))
    ;; Формируем аргументы команды
    (when quiet
      (push "-q" args))
    (when force
      (push "-f" args))
    (when merge
      (push "-m" args))
    (when detach
      (push "--detach" args))
    (when create
      (push "-b" args))
    (when create-force
      (push "-B" args))
    (when orphan
      (push "--orphan" args))
    (when ours
      (push "--ours" args))
    (when theirs
      (push "--theirs" args))
    (when conflict
      (push (format nil "--conflict=~A" conflict) args))
    (when patch
      (push "-p" args))
    ;; Добавляем target
    (when target
      (push target args))
    ;; Добавляем start-point, если указан
    (when start-point
      (push start-point args))
    ;; Добавляем pathspec, если указан
    (when pathspec
      (push "--" args)
      (if (listp pathspec)
          (dolist (path pathspec)
            (push path args))
          (push pathspec args)))
    ;; Переворачиваем список аргументов
    (setf args (nreverse args))
    ;; Выполняем команду
    (multiple-value-bind (out err code)
        (apply #'cl-git-tree/git-utils:git-run (<workspace>-path ws) args)
      (cond
        ((zerop code)
         (cond
           (pathspec
            (format t "~A [~A] Восстановлены файлы~@[ из ~A~]~%"
                    (find-emo ws "git checkout")
                    (repo-name ws)
                    target))
           (t
            (format t "~A [~A] Переключено на: ~A~%"
                    (find-emo ws "git checkout")
                    (repo-name ws)
                    target)))
         (when (and out (> (length out) 0) (not quiet))
           (format t "~A~%" out))
         t)
        (t
         (format t "~A Ошибка при выполнении git checkout в ~A~%"
                 (find-emo ws "error")
                 (repo-name ws))
         (when err
           (format t "  ~A~%" err))
         nil)))))
