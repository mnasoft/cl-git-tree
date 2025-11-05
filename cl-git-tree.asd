(defsystem "cl-git-tree"
  :description "Git location manager and sync tool in Common Lisp"
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-truename* "README.org"))
  :version "0.2.2"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GPL-3.0-or-later"
  :depends-on (
               ;;"cl-git"
               "cl-fad"
               "uiop"
               "alexandria"
               "split-sequence"
               "cl-git-tree/git-utils"
               "cl-git-tree/loc"
               "cl-git-tree/fs"
               "cl-git-tree/dispatch"
               "cl-git-tree/global"
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
       (:file "all")
       (:file "clone")
       (:file "unclone")
       (:file "remote-add")
       (:file "remote-remove")
       (:file "remote-readd")
       (:file "remake-xz")
       (:file "info")
       (:file "status")
       (:file "aliases")))
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

(defsystem "cl-git-tree/fs"
  :description "Файловая подсистема для поиска и обхода git-репозиториев."
  :depends-on (:uiop :cl-git-tree/loc)
  :serial t
  :components
  ((:module "src/fs"
    :components
    ((:file "package")
     (:file "fs")))))

(defsystem "cl-git-tree/loc"
  :description "Подсистема для работы с location-шаблонами (хранилище, поиск, фильтрация)."
  :depends-on (:uiop :cl-ppcre)
  :serial t
  :components
  ((:module "src/loc"
    :components
    ((:file "package")
     (:file "location")))))

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
  :serial t
  :components
  ((:module "src/global"
    :components ((:file "package")
                 (:file "git-global")))))

