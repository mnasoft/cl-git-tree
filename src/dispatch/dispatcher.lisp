;;;; ./src/dispatcher.lisp

(in-package :cl-git-tree/dispatch)

(defparameter *commands* nil
  "Глобальный список зарегистрированных CLI‑команд.

Каждый элемент списка имеет вид:
  (NAME FUNCTION DESCRIPTION)

  NAME        — строка, имя команды (например, \"clone\").
  FUNCTION    — функция, реализующая команду.
  DESCRIPTION — краткая строка для справки (используется в SHOW-HELP).")

(defun register-command (name fn description)
  "Регистрирует новую CLI‑команду в *commands*.

Аргументы:
  NAME        — строка, имя команды.
  FN          — функция, реализующая команду.
  DESCRIPTION — краткое описание для справки.

Пример:
  (register-command \"version\" #'show-version \"Показать версию\")"
  (push (list name fn description) *commands*))

(defun show-help ()
  "Выводит список всех зарегистрированных команд и их описания."
  (format t "Usage: git-tree <command> [args...]~%~%")
  (format t "Доступные команды:~%")
  (dolist (cmd (reverse *commands*)) ;; reverse, чтобы показать в порядке регистрации
    (destructuring-bind (name fn description) cmd
      (declare (ignore fn))
      (format t "  ~A~20T - ~A~%" name description))))

(defun show-version ()
  "Выводит текущую версию системы cl-git-tree."
  (let* ((sys (asdf:find-system :cl-git-tree))
         (ver (asdf:component-version sys)))
    (format t "cl-git-tree version ~A~%" ver)))

;; Регистрация встроенных команд
(register-command "version" #'show-version "Показать версию")
(register-command "help"    #'show-help    "Эта справка")

(defun dispatch-command (cmd args)
  "Находит команду по имени CMD и вызывает её с аргументами ARGS.
Если команда не найдена, выводит сообщение об ошибке."
  (let ((entry (assoc cmd *commands* :test #'string=)))
    (if entry
        (apply (second entry) args)
        (format t "Неизвестная команда: ~A~%" cmd))))
