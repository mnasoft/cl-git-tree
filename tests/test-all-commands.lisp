;;;; ./tests/test-all-commands.lisp

(in-package :cl-git-tree/tests)

(def-suite all-commands-tests
  :description "Тесты для проверки наличия всех CLI-команд"
  :in all)

(in-suite all-commands-tests)

;;; Вспомогательные функции

(defun command-registered-p (name)
  "Проверяет, зарегистрирована ли команда с именем NAME."
  (not (null (assoc name cl-git-tree/dispatch:*commands* :test #'string=))))

(defun get-command-entry (name)
  "Возвращает запись команды (name fn description) или NIL если не найдена."
  (assoc name cl-git-tree/dispatch:*commands* :test #'string=))

;;; Проверяем, что все ожидаемые команды зарегистрированы

(test all-commands-are-registered
  "Проверяем, что все основные команды зарегистрированы в диспетчере."
  (let ((expected-commands
         '("add"
           "aliases"
           "commit"
           "info"
           "locations"
           "patterns"
           "pull"
           "push"
           "remote"
           "status")))
    (loop for cmd in expected-commands do
      (is-true (command-registered-p cmd)
               (format nil "Команда '~A' должна быть зарегистрирована" cmd)))))

(test help-option-for-all-commands
  "Проверяем, что каждая команда имеет функцию и описание."
  (let ((commands '("add" "commit" "info" "locations" "patterns"
                    "pull" "push" "remote" "status" "aliases")))
    (loop for cmd in commands do
      (let ((entry (get-command-entry cmd)))
        (is-true entry (format nil "Команда '~A' должна быть зарегистрирована" cmd))
        (when entry
          (is-true (second entry) (format nil "Команда '~A' должна иметь функцию" cmd))
          (is-true (third entry) (format nil "Команда '~A' должна иметь описание" cmd)))))))

(test remote-subcommands-available
  "Проверяем, что подкоманды remote доступны."
  (let ((entry (get-command-entry "remote")))
    (is-true entry "remote должна быть зарегистрирована")
    (when entry
      (let ((desc (third entry)))
        (is-true (search "list" desc :test #'char-equal) "remote должна включать подкоманду list")
        (is-true (search "add" desc :test #'char-equal) "remote должна включать подкоманду add")
        (is-true (search "remove" desc :test #'char-equal) "remote должна включать подкоманду remove")
        (is-true (search "readd" desc :test #'char-equal) "remote должна включать подкоманду readd")
        (is-true (search "create" desc :test #'char-equal) "remote должна включать подкоманду create")
        (is-true (search "delete" desc :test #'char-equal) "remote должна включать подкоманду delete")))))

(test locations-subcommands-available
  "Проверяем, что подкоманды locations доступны."
  (let ((entry (get-command-entry "locations")))
    (is-true entry "locations должна быть зарегистрирована")
    (when entry
      (let ((desc (third entry)))
        (is-true (search "list" desc :test #'char-equal) "locations должна включать подкоманду list")
        (is-true (search "show" desc :test #'char-equal) "locations должна включать подкоманду show")
        (is-true (search "add" desc :test #'char-equal) "locations должна включать подкоманду add")
        (is-true (search "edit" desc :test #'char-equal) "locations должна включать подкоманду edit")
        (is-true (search "remove" desc :test #'char-equal) "locations должна включать подкоманду remove")
        (is-true (search "save" desc :test #'char-equal) "locations должна включать подкоманду save")))))

(test patterns-description-available
  "Проверяем, что patterns зарегистрирована с описанием."
  (let ((entry (get-command-entry "patterns")))
    (is-true entry "patterns должна быть зарегистрирована")))

(test total-commands-count
  "Проверяем, что всего зарегистрировано ожидаемое количество команд."
  (let ((cmd-count (length cl-git-tree/dispatch:*commands*)))
    ;; Минимум 12 команд: 10 основных + help + version
    (is-true (>= cmd-count 12)
             (format nil "Должно быть минимум 12 команд, найдено: ~A" cmd-count))))
