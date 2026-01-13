;;;; ./src/loc/methods/repo-switch.lisp

(in-package :cl-git-tree/loc)

(defmethod repo-switch ((ws <workspace>) branch
                        &key detach create create-force orphan no-guess start-point
                        &allow-other-keys)
  "Переключиться на ветку BRANCH в рабочем пространстве WS.
Аргументы:
  WS          → рабочее пространство (<workspace>)
  BRANCH      → имя ветки для переключения
Ключи:
  :detach     → T → отсоединённое состояние HEAD (--detach)
  :create     → T → создать новую ветку (-c)
  :create-force → T → создать или пересоздать ветку (-C)
  :orphan     → T → создать ветку-сироту без истории (--orphan)
  :no-guess   → T → не пытаться угадать удалённую ветку (--no-guess)
  :start-point → начальная точка для новой ветки (коммит/ветка/тег)"
  (let* ((args (list "switch")))
    ;; Формируем аргументы команды
    (when detach
      (push "--detach" args))
    (when create
      (push "-c" args))
    (when create-force
      (push "-C" args))
    (when orphan
      (push "--orphan" args))
    (when no-guess
      (push "--no-guess" args))
    ;; Добавляем имя ветки
    (when branch
      (push branch args))
    ;; Добавляем start-point, если указан
    (when start-point
      (push start-point args))
    ;; Переворачиваем список аргументов (так как добавляли через push)
    (setf args (nreverse args))
    ;; Выполняем команду
    (multiple-value-bind (out err code)
        (apply #'cl-git-tree/git-utils:git-run (<workspace>-path ws) args)
      (cond
        ((zerop code)
         (format t "~A [~A] Переключено на ветку: ~A~%"
                 (find-emo ws "git switch")
                 (repo-name ws)
                 branch)
         (when (and out (> (length out) 0))
           (format t "~A~%" out))
         t)
        (t
         (format t "~A Ошибка при переключении на ветку '~A' в ~A~%"
                 (find-emo ws "error")
                 branch
                 (repo-name ws))
         (when err
           (format t "  ~A~%" err))
         nil)))))
