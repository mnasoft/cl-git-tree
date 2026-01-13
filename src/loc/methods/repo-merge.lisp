;;;; ./src/loc/methods/repo-merge.lisp

(in-package :cl-git-tree/loc)

(defmethod repo-merge ((ws <workspace>) source
                       &key no-commit squash no-edit no-verify strategy
                            strategy-option keyid allow-unrelated-histories
                            no-rerere-autoupdate message message-file
                            into-name stat no-stat compact-summary
                            continue abort quit
                       &allow-other-keys)
  "Объединить две или более веток разработки.
Аргументы:
  WS          → рабочее пространство (<workspace>)
  SOURCE      → ветка/коммит для слияния
Ключи:
  :no-commit  → T → не создавать коммит автоматически (--no-commit)
  :squash     → T → сжать все коммиты в один (--squash)
  :no-edit    → T → не редактировать сообщение коммита (--no-edit)
  :no-verify  → T → пропустить pre-merge и commit-msg хуки (--no-verify)
  :strategy   → стратегия слияния (-s, например \"ours\", \"recursive\")
  :strategy-option → опции стратегии (-X, например \"theirs\")
  :keyid      → GPG ключ для подписи (-S)
  :allow-unrelated-histories → T → разрешить несвязанные истории (--allow-unrelated-histories)
  :no-rerere-autoupdate → T → не обновлять rerere автоматически (--no-rerere-autoupdate)
  :message    → сообщение коммита (-m)
  :message-file → файл с сообщением коммита (-F)
  :into-name  → имя ветки для слияния (--into-name)
  :stat       → T → показать диффстат (--stat)
  :no-stat    → T → не показывать диффстат (--no-stat)
  :compact-summary → T → компактная сводка (--compact-summary)
  :continue   → T → продолжить после разрешения конфликтов (--continue)
  :abort      → T → прервать слияние (--abort)
  :quit       → T → выйти из процесса слияния (--quit)"
  (let* ((args (list "merge")))
    ;; Формируем аргументы команды
    (cond
      (continue (push "--continue" args))
      (abort (push "--abort" args))
      (quit (push "--quit" args))
      (t
       ;; Обычное слияние
       (when no-commit
         (push "--no-commit" args))
       (when squash
         (push "--squash" args))
       (when no-edit
         (push "--no-edit" args))
       (when no-verify
         (push "--no-verify" args))
       (when strategy
         (push "-s" args)
         (push strategy args))
       (when strategy-option
         (push "-X" args)
         (push strategy-option args))
       (when keyid
         (push (if (stringp keyid)
                   (format nil "-S~A" keyid)
                   "-S")
               args))
       (when allow-unrelated-histories
         (push "--allow-unrelated-histories" args))
       (when no-rerere-autoupdate
         (push "--no-rerere-autoupdate" args))
       (when stat
         (push "--stat" args))
       (when no-stat
         (push "--no-stat" args))
       (when compact-summary
         (push "--compact-summary" args))
       (when message
         (push "-m" args)
         (push message args))
       (when message-file
         (push "-F" args)
         (push message-file args))
       (when into-name
         (push "--into-name" args)
         (push into-name args))
       ;; Добавляем source для слияния
       (when source
         (push source args))))
    ;; Переворачиваем список аргументов
    (setf args (nreverse args))
    ;; Выполняем команду
    (multiple-value-bind (out err code)
        (apply #'cl-git-tree/git-utils:git-run (<workspace>-path ws) args)
      (cond
        ((zerop code)
         (cond
           (continue
            (format t "~A [~A] Слияние продолжено~%"
                    (find-emo ws "git merge")
                    (repo-name ws)))
           (abort
            (format t "~A [~A] Слияние прервано~%"
                    (find-emo ws "git merge")
                    (repo-name ws)))
           (quit
            (format t "~A [~A] Выход из процесса слияния~%"
                    (find-emo ws "git merge")
                    (repo-name ws)))
           (t
            (format t "~A [~A] Слияние ~A успешно завершено~%"
                    (find-emo ws "git merge")
                    (repo-name ws)
                    source)))
         (when (and out (> (length out) 0))
           (format t "~A~%" out))
         t)
        (t
         (format t "~A Ошибка при выполнении git merge в ~A~%"
                 (find-emo ws "error")
                 (repo-name ws))
         (when err
           (format t "  ~A~%" err))
         nil)))))
