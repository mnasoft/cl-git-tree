(defsystem "cl-git-tree-tests"
  :description "Тесты для cl-git-tree (global, workspace, loc, commands, fs, git-utils)"
  :version "0.2.2"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GPL-3.0-or-later"

  :depends-on (:cl-git-tree
               :fiveam)
  :perform (test-op (o s)
		    (uiop:symbol-call :cl-git-tree/tests :run-tests))
  :serial t
  :components
  ((:module "tests"
    :components
    ((:file "package")
     (:file "all")
     (:file "test-global")
     (:file "test-location")
     (:module "workspace"
      :serial t
      :components
      ((:file "workspace")
       (:file "make-instance")
       (:file "make-workspace")))


  
     ;; (:file "test-commands")
     ;; (:file "test-fs")
     ;; (:file "test-git-utils")
     ;; (:file "run")
     ))))
