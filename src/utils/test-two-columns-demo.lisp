;;; Демонстрация функции двухколоночного вывода

(require 'asdf)
(asdf:load-system :cl-git-tree/utils)

(use-package :cl-git-tree/utils/two-columns)

;; Список команд из README.org
(defparameter *commands-list*
  '("add"
    "clone"
    "commit"
    "pull"
    "push"
    "all"
    "remote"
    "locations"
    "patterns"
    "aliases"
    "info"
    "transport"
    "audit"
    "remake-xz"
    "help"
    "version"))

(defparameter *locations-examples*
  '("gh - GitHub"
    "gl - GitLab"
    "lc - Local Cache"
    "mirror1"
    "mirror2"
    "backup-local"
    "production-server"
    "staging-server"
    "dev-server"
    "archive"))

;; Вывод команд в две колонки
(format t "~%=== ДОСТУПНЫЕ КОМАНДЫ (две колонки) ===~%~%")
(print-two-columns *commands-list* :column-width 25 :separator "   ")

;; Вывод примеров локаций
(format t "~%=== ПРИМЕРЫ ЛОКАЦИЙ (две колонки) ===~%~%")
(print-two-columns *locations-examples* :column-width 30 :separator "   ")

;; Вывод с большей шириной
(format t "~%=== МОДУЛИ ПРОЕКТА (две колонки, большая ширина) ===~%~%")
(print-two-columns '("cli"
                     "config"
                     "package"
                     "commands"
                     "dispatch"
                     "loc"
                     "fs"
                     "git-utils"
                     "shell-utils"
                     "config"
                     "global"
                     "tests"
                     "workspace"
                     "utils")
                   :column-width 35 :separator "    ")
