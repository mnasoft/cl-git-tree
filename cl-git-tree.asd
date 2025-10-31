(defsystem "cl-git-tree"
  :description "Git location manager and sync tool in Common Lisp"
  :version "0.1.9"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("cl-git"
               "uiop"
               "alexandria"
               "split-sequence"
               "cl-git-tree/git-utils"
               )
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "config")
                 (:file "location")
                 (:module "fs" 
                  :components
                  ((:file "package")
                   (:file "fs"))) 
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
                   (:file "aliases")
                   ))
                 (:file "dispatcher")
                 (:file "cli")))))


(defsystem "cl-git-tree/git-utils"
  :description "Git utility functions for cl-git-tree"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on (:uiop)
  :serial t
  :components ((:file "src/git-utils")))
