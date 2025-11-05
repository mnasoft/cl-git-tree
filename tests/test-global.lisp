#+nil
(defpackage :cl-git-tree/tests/global
  (:use :cl :fiveam
        :cl-git-tree/global))
#+nil
(in-package :cl-git-tree/tests/global)

(in-package :cl-git-tree/tests)

(def-suite global
  :description "Тесты для подсистемы cl-git-tree/global"
  :in all)

(in-suite global)

(def-test config-set-and-get ()
  "Проверяем, что установка и получение значения работают."
  (let ((key "user.testname")
        (value "Микола-Tester"))
    ;; Установим значение
    (git-config-set cl-git-tree/global:*git-global* key value)
    ;; Проверим, что оно читается
    (is-true (string= (git-config-get *git-global* key) value))))

(def-test config-list-contains ()
  "Проверяем, что список настроек содержит установленный ключ."
  (let ((key "user.testname")
        (value "Микола-Tester"))
    (git-config-set cl-git-tree/global:*git-global* key value)
    (let ((all (git-config-list *git-global*)))
      (is-true (find (format nil "~A=~A" key value) all :test #'string=)))))

