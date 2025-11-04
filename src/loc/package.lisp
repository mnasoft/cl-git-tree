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
   <location>-local
   <location>-description

   ;; глобальная таблица
   *locations* 

   ;; функции
   add-location
   find-location
   location-exists-p
   all-location-keys
   all-locations
   repo-url
   print-locations
   infer-local-p
   location-local-p
   match-location-keys)
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

