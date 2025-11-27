(in-package :cl-git-tree/loc)

(defmethod repo-commit ((ws <workspace>)
                        &key
                          (message
                           (multiple-value-bind (sec min hour day mon year)
                               (decode-universal-time (get-universal-time))
                             (format nil "commit ~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                                     year mon day hour min sec)))
                          all
                          amend)
  "Выполнить коммит в рабочем пространстве WS.
Поддерживаются ключи:
  -a / :all    → добавить все изменённые файлы
  --amend      → изменить последний коммит
  -m / :message → сообщение коммита"
  (let ((args (list "commit")))
    ;; добавляем ключи в список аргументов
    (when all
      (push "-a" args))
    (when amend
      (push "--amend" args))
    (when message
      (push "-m" args)
      (push message args))
    ;; запускаем git
    (multiple-value-bind (out err code)
        (apply #'cl-git-tree/git-utils:git-run
               (<workspace>-path ws)
               (nreverse args))
      (if (= code 0)
          out
          (format nil "Ошибка git commit (код ~A)~%~A" code err)))))


