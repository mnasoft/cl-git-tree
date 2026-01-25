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
    repo-push-all
   repo-pull
    repo-pull-all
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
  location->arg-string
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
   repo-transport-export-all
   repo-transport-import
   repo-transport-unpack
   expand-path
   repo-status
   repo-commit
   repo-branches
   repo-push
    repo-push-all
   repo-pull
    repo-pull-all
   repo-fetch
   repo-merge
   repo-switch
   repo-checkout
   repo-ls-remote
   repo-branch
   ;; конструктор и утилиты
   make-workspace
   detect-os
   )
  (:export
   find-emo
   )
  (:documentation
   " @b(Пакет:) Управление локациями и рабочими пространствами.

 @b(Назначение:) Инкапсулирует подсистему регистрации, поиска и
использования шаблонов локаций (git-URL, архивы, локальные
пути). Позволяет строить полные URL репозиториев и управлять рабочими
пространствами для разных операционных систем.

 @b(Ключевые компоненты:)
@begin(list)
 @item(<location> - класс для описания шаблона локации с URL-git,
 URL-xz, провайдером;)
 @item(*locations* - глобальная таблица всех зарегистрированных
локаций;)
 @item(add-location, find-location, all-locations - функции управления
 локациями;)
 @item(<workspace> - класс для описания рабочего пространства в
 репозитории;)
 @item(Generic-функции: repo-add, repo-push, repo-pull, remote-create
 и др.;)

@end(list)

@b(Пример:)
@begin[lang=lisp](code)
 ;; Добавить локацию GitHub
 (add-location \"gh\"
   :url-git \"git@github.com:username/\"
   :provider :github)

 ;; Получить URL репозитория
 (repo-url \"gh\" \"cl-git-tree\")
 ;; => \"git@github.com:username/cl-git-tree.git\"

 ;; Создать рабочее пространство
 (make-workspace #P\"/path/to/repo\")
@end(code)"))

(in-package :cl-git-tree/loc)
