;;;; ./tests/test-dispatch.lisp

(in-package :cl-git-tree/tests)

(def-suite dispatch-tests
  :description "Тесты для подсистемы cl-git-tree/dispatch"
  :in all)

(in-suite dispatch-tests)

(def-test register-command-adds-to-registry ()
  "Проверяем, что register-command добавляет команду в *commands*."
  (let ((original-commands cl-git-tree/dispatch:*commands*))
    (unwind-protect
         (progn
           (cl-git-tree/dispatch:register-command
            "test-cmd"
            (lambda (&rest args) (format nil "test: ~S" args))
            "Тестовая команда")
           (let ((found (assoc "test-cmd" cl-git-tree/dispatch:*commands* :test #'string=)))
             (is-true (not (null found)))
             (is-true (string= (first found) "test-cmd"))
             (is-true (string= (third found) "Тестовая команда"))))
      ;; restore original commands
      (setf cl-git-tree/dispatch:*commands* original-commands))))

(def-test dispatch-command-calls-registered-function ()
  "Проверяем, что dispatch-command вызывает зарегистрированную функцию."
  (let ((original-commands cl-git-tree/dispatch:*commands*)
        (result nil))
    (unwind-protect
         (progn
           (cl-git-tree/dispatch:register-command
            "test-dispatch"
            (lambda (&rest args) (setf result args))
            "Test dispatch")
           (cl-git-tree/dispatch:dispatch-command "test-dispatch" '("arg1" "arg2"))
           (is-true (equal result '("arg1" "arg2"))))
      (setf cl-git-tree/dispatch:*commands* original-commands))))

(def-test dispatch-command-handles-unknown-command ()
  "Проверяем, что неизвестная команда не вызывает ошибку."
  (let ((output (with-output-to-string (*standard-output*)
                  (cl-git-tree/dispatch:dispatch-command "nonexistent-cmd" '()))))
    (is-true (search "Неизвестная команда" output))))

(def-test show-version-outputs-version ()
  "Проверяем, что show-version выводит версию системы."
  (let ((output (with-output-to-string (*standard-output*)
                  (cl-git-tree/dispatch:show-version))))
    (is-true (search "cl-git-tree version" output))))

(def-test show-help-lists-commands ()
  "Проверяем, что show-help выводит список команд."
  (let ((output (with-output-to-string (*standard-output*)
                  (cl-git-tree/dispatch:show-help))))
    (is-true (search "Usage:" output))
    (is-true (search "Доступные команды:" output))))
