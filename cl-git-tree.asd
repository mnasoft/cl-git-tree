(defsystem "cl-git-tree"
  :description "Git location manager and sync tool in Common Lisp"
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-truename* "README.org"))
  :version "0.6.9"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GPL-3.0-or-later"
  :depends-on (
               ;;"cl-git"
               "cl-fad"
               "uiop"
               "alexandria"
               "split-sequence"
               "cl-ppcre"
               "cl-git-tree/git-utils"
               "cl-git-tree/loc"
               "cl-git-tree/fs"
               "cl-git-tree/dispatch"
               "cl-git-tree/global"
               "cl-git-tree/config"
               "cl-git-tree/shell-utils"
               "cl-git-tree/utils"
               )
  :serial t
  :in-order-to ((test-op (test-op "cl-git-tree-tests")))
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "config")
     (:module "commands"
      :serial t
      :components
      ((:file "add")
       (:file "commit")
       (:file "pull")
       (:file "push")
       (:file "remote-delete")
       (:file "remote-add")
       (:file "remote-remove")
      (:file "remote-readd")
      (:file "remote-create")
       (:file "remote")
       (:file "remake-xz")
      (:file "patterns")
      (:file "locations")
      (:file "audit")
      (:file "transport")
      (:file "transport-clean")
      (:file "transport-import")
      (:file "transport-export")
      (:file "aliases")
      (:module "single-repo"
       :serial t
       :components
       ((:file "package")
        (:file "branch")
        (:file "switch")
        (:file "checkout")
        (:file "ls-remote")
        (:file "fetch")
        (:file "merge")))))
     (:file "cli")))))


(defsystem "cl-git-tree/git-utils"
  :description "Git utility functions for cl-git-tree"
  :depends-on (:uiop)
  :serial t
  :components
  ((:module "src/git-utils"
    :components
    ((:file "package")
     (:file "git-utils")))))

(defsystem "cl-git-tree/shell-utils"
  :description "Утилиты для запуска произвольных CLI-команд из Common Lisp"
  :depends-on (:uiop :cl-ppcre)
  :components
  ((:module "src/shell-utils"
    :components
    ((:file "package")
     (:file "core")
     (:file "ssh-utils")))))

(defsystem "cl-git-tree/fs"
  :description "Файловая подсистема для поиска и обхода git-репозиториев."
  :depends-on (:uiop
               :cl-fad
               :cl-git-tree/shell-utils
               ;; :cl-git-tree/loc
               )
  :serial t
  :components
  ((:module "src/fs"
    :components
    ((:file "package")
     (:file "fs")))))

(defsystem "cl-git-tree/config"
  :description "Подсистема конфигурации: дефолтные паттерны включения и исключения."
  :depends-on (:uiop)
  :serial t
  :components
  ((:module "src/config"
    :components
    ((:file "package")
     (:file "defaults")
     (:file "file-patterns")))))

(defsystem "cl-git-tree/loc"
  :description "Подсистема для описания локаций и провайдеров (local, github, gitlab) и связанных операций."
  :depends-on ("uiop"
               "cl-ppcre"
               "cl-git-tree/fs"
               "cl-git-tree/git-utils"
               "cl-git-tree/shell-utils"
               "cl-git-tree/emodji")
  :serial t
  :components
  ((:module "src/loc"
    :components
    ((:file "package")
     (:file "defgeneric")   ; объявления generic-функций
     (:file "defclass")     ; базовые классы <location>, <provider>
     (:file "provider")     ; специализированные <local>, <github>, <gitlab>
     (:file "location")     ; вспомогательные функции для работы с локациями
     (:file "workspace") 
     (:module "methods"     ; реализация методов по generic
      :components
      ((:file "initialize-instance")
       (:file "print-object")
       (:file "remote-create")
       (:file "remote-delete")
       (:file "repo-push")
       (:file "repo-pull")
       (:file "repo-add")
       (:file "repo-branch")
       (:file "repo-branches")
       (:file "repo-ls-remote")
       (:file "repo-fetch")
       (:file "repo-merge")
       (:file "repo-switch")
       (:file "repo-checkout")
       (:file "repo-status")
       (:file "repo-commit")
       (:file "repo-name")
      (:file "repo-is-clean-p")
      (:file "repo-last-commit-date")
      (:file "days-since-last-commit")       (:file "expand-path")      (:file "repo-provider-keys")
      (:file "repo-providers")
      (:file "repo-transport-export")
      (:file "repo-transport-import")
      (:file "repo-transport-unpack")
      (:file "remote-import-connect")
      (:file "remote-import-disconnect")
        (:file "remote-import-cleanup-dir")
        (:file "remote-import-delete-archive")
       (:file "remote-url")
       (:file "remote-add")
       (:file "remote-readd")
          (:file "remote-remove")
          (:file "find-emo")
       ))))))

(defsystem "cl-git-tree/dispatch"
  :description "Подсистема диспетчера CLI-команд для cl-git-tree"
  :depends-on ("uiop" "asdf")
  :serial t
  :components
  ((:module "src/dispatch"
    :components
    ((:file "package")
     (:file "dispatcher")))))

(defsystem "cl-git-tree/global"
  :description "Подсистема для работы с глобальной конфигурацией Git (git config --global)."
  :depends-on (:cl-git-tree/git-utils   ; используем git-run
               :split-sequence)    ; для парсинга вывода config --list
  :components
  ((:module "src/global"
    :serial t
    :components
    ((:file "package")
     (:file "git-global")))))

(defsystem "cl-git-tree/utils"
  :description "Утилиты для форматирования и вывода (двухколоночный формат и т.д.)"
  :serial t
  :components
  ((:module "src/utils"
    :components
    ((:file "two-columns")))))

(defsystem "cl-git-tree/emodji"
  :description "Unified monochrome + color symbol table for Git commands"
  :version "0.1.0"
  :serial t
  :components
  ((:module "src/emodji"
    :components
    ((:file "package")
     (:file "core")
     (:file "emodji-status")
     (:file "emodji-git")
     (:file "emodji-fs")
     
     ))))
  

