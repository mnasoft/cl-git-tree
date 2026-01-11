(defpackage :cl-git-tree/tests/commands
  (:use :cl :fiveam
        :cl-git-tree/commands
        :cl-git-tree/workspace
        :cl-git-tree/global))

(in-package :cl-git-tree/tests/commands)

(defsuite commands-tests
  :description "Тесты для CLI-команд (status, commit, config и т.п.)")

(in-suite commands-tests)

;; --- Пример теста для config ---

(test cmd-config-get-and-set
  "Проверяем CLI-команду config (get/set)."
  (let ((key "user.testcli")
        (value "CLI-Tester"))
    ;; вызываем CLI-обёртку, которая внутри дергает git-config-set
    (is (string= (cmd-config '("set" key value)) value))
    (is (string= (cmd-config `("get" ,key)) value))))
