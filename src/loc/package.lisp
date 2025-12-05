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
   repo-clone

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
   ;; аксессоры
   <workspace>-path
   <workspace>-description 
   
   ;; generic-функции
   git-init
   git-initialized-p
   git-root
   repo-name
   repo-status
   repo-commit
   repo-branches
   ;; конструктор
   make-workspace)
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
