;;;; ./src/dispatcher.lisp

(defpackage :cl-git-tree/dispatch
  (:use :cl)
  (:export *commands*
           register-command
           dispatch-command
           show-help
           show-version)
  (:documentation
   "Подсистема диспетчера CLI-команд для cl-git-tree.

Экспортируемые символы:
  *COMMANDS*        — глобальный список зарегистрированных команд.
  REGISTER-COMMAND  — функция для добавления новой команды.
  DISPATCH-COMMAND  — вызов команды по имени.
  SHOW-HELP         — выводит список всех команд.
  SHOW-VERSION      — показывает версию системы."))

(in-package :cl-git-tree/dispatch)
