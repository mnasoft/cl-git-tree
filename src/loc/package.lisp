;;;; ./src/loc/package.lisp

(defpackage :cl-git-tree/loc
  (:use :cl)
  (:export
   ;; класс
   <location>
   
   ;; аксессоры
   <location>-id
   <location>-url-git
   <location>-url-xz
   <location>-tar
   <location>-provider
   <location>-description

   ;; класс
   <provider>
   <local>
   <github>
   <gitlab>

   ;; обобщенные функции
   clone
   repo-add
   remote-create
   repo-push
   repo-pull
   remote-delete
   remote-add
   remote-remove
   remote-readd
   remote-import-connect
   remote-import-disconnect
   remote-import-cleanup-dir
   remote-import-delete-archive

   ;; глобальная таблица
   *locations* 

   ;; функции
   register-location
   add-location
   remove-location
   save-locations-config
   find-location
   location-exists-p
   all-location-keys
   all-locations
   repo-url
   print-locations
   infer-local-p
   location-local-p
   match-location-keys)

  (:export
   ;; класс
   <workspace>
   <workspace-linux>
   <workspace-windows>
   <workspace-msys2>
   ;; аксессоры
   <workspace>-path
   <workspace>-description
   <workspace>-os-type
   
   ;; generic-функции
   git-init
   git-initialized-p
   git-root
   repo-name
   repo-is-clean-p
   repo-last-commit-date
   days-since-last-commit
   repo-provider-keys
   repo-providers
   repo-transport-export
   repo-transport-import
   repo-transport-unpack
   expand-path
   repo-status
   repo-commit
   repo-branches
   repo-push
   repo-pull
   repo-fetch
   repo-merge
   repo-switch
   repo-checkout
   repo-ls-remote
   ;; конструктор и утилиты
   make-workspace
   detect-os
   )
  (:export
   find-emo
   )
  (:documentation
   "Пакет CL-GIT-TREE/LOC инкапсулирует подсистему управления локациями.

Экспортируемые сущности:
  • Класс <location> и его аксессоры (<location>-name, <location>-url-git и др.)
  • Глобальная таблица *locations* для хранения всех зарегистрированных локаций
  • Функции для работы с локациями:
      - add-location / find-location / location-exists-p
      - all-location-keys / all-locations
      - repo-url / print-locations
      - infer-local-p / location-local-p
      - match-location-keys

Назначение:
  Подсистема обеспечивает регистрацию, поиск и использование шаблонов
  локаций (git-URL, архивы, локальные пути). Используется другими
  компонентами cl-git-tree для построения URL, проверки локальности
  и обхода всех доступных локаций."))

(in-package :cl-git-tree/loc)
