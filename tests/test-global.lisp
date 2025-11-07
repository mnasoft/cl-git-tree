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
    (cl-git-tree/global:git-config-set
     cl-git-tree/global:*git-global* key value)
    ;; Проверим, что оно читается
    (is-true
     (string=
      (cl-git-tree/global:git-config-get
       cl-git-tree/global:*git-global* key)
      value))))

(def-test config-list-contains ()
  "Проверяем, что список настроек содержит установленный ключ."
  (let ((key "user.testname")
        (value "Микола-Tester"))
    (cl-git-tree/global:git-config-set cl-git-tree/global:*git-global* key value)
    (let ((all (cl-git-tree/global:git-config-list cl-git-tree/global:*git-global*)))
      (is-true (find (format nil "~A=~A" key value) all :test #'string=)))))


(def-test config-unset-removes-key ()
  "Проверяем, что git-config-unset удаляет значение ключа."
  (let ((key "user.testname")
        (value "Микола-Tester"))
    ;; Установим значение
    (cl-git-tree/global:git-config-set cl-git-tree/global:*git-global* key value)
    ;; Убедимся, что оно читается
    (is-true (string= (cl-git-tree/global:git-config-get cl-git-tree/global:*git-global* key) value))
    ;; Удалим ключ
    (is-true (cl-git-tree/global:git-config-unset cl-git-tree/global:*git-global* key))
    ;; Теперь git-config-get должен вернуть пустую строку
    (is-true (string= (cl-git-tree/global:git-config-get cl-git-tree/global:*git-global* key) ""))))

