(defsystem "cl-git-tree-tests"
  :description "Тесты для cl-git-tree (global, workspace, loc, commands, fs, git-utils)"
  :version "0.2.2"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GPL-3.0-or-later"

  :depends-on (:cl-git-tree
               :fiveam)
  :perform (test-op (o s)
		    (uiop:symbol-call :cl-git-tree/tests :run-tests))
  :components
  ((:module "tests"
    :serial t
    :components
    ((:file "package")
     (:file "all")
     (:file "test-global")
     ;; (:file "test-workspace")
     ;; (:file "test-location")
     ;; (:file "test-commands")
     ;; (:file "test-fs")
     ;; (:file "test-git-utils")
     ;; (:file "run")
     ))))
