(defsystem "cl-git-tree-tests"
  :description "Тесты для cl-git-tree (global, workspace, loc, commands, fs, git-utils)"
  :version "0.2.4"
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
     (:file "test-patterns")
     (:file "test-location")
     (:file "test-dispatch")
     (:file "test-all-commands")
     (:file "test-shell-utils")
     (:file "test-os-detection")
     (:file "test-work-tests")
     (:file "test-remote-delete-msys2-local")
     ;; (:file "test-remote-create-delete-lc")  ;; временно отключён - использует неправильный синтаксис
     (:module "workspace"
      :serial t
      :components
      ((:file "workspace")
       (:file "make-instance")
       (:file "make-workspace")))
     ;; (:file "test-cmd-commit-tree")
     ;; (:file "test-commands")
     ;; (:file "test-fs")
     ;; (:file "test-git-utils")
     ;; (:file "run")
     ))))
