(defpackage :cl-git-tree/tests/git-utils
  (:use :cl :fiveam
        :cl-git-tree/git-utils))

(in-package :cl-git-tree/tests/git-utils)

(defsuite git-utils-tests
  :description "Тесты для подсистемы cl-git-tree/git-utils")

(in-suite git-utils-tests)

(test git-run-version
  "Проверяем, что git-run возвращает версию git."
  (multiple-value-bind (out err code)
      (git-run nil "--version")
    (is (= code 0))
    (is (search "git version" out))
    (is (string= err ""))))

(test git-run-init-and-status
  "Создаём временный репозиторий и проверяем git status."
  (uiop:with-temporary-directory (tmpdir)
    ;; инициализируем репозиторий
    (multiple-value-bind (out err code)
        (git-run tmpdir "init")
      (is (= code 0))
      (is (string= err "")))
    ;; проверяем статус
    (multiple-value-bind (out err code)
        (git-run tmpdir "status")
      (is (= code 0))
      (is (search "On branch" out))))))

(test git-run-error-code
  "Проверяем, что неверная команда возвращает ненулевой код."
  (multiple-value-bind (out err code)
      (git-run nil "not-a-command")
    (is (/= code 0))
    (is (string= out ""))     ;; stdout пустой
    (is (plusp (length err))))) ;; stderr содержит сообщение
