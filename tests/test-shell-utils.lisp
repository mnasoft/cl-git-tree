;;;; ./tests/test-shell-utils.lisp

(in-package :cl-git-tree/tests)

(def-suite shell-utils-tests
  :description "Тесты для подсистемы cl-git-tree/shell-utils"
  :in all)

(in-suite shell-utils-tests)

(def-test parse-ssh-url-extracts-components ()
  "Проверяем, что parse-ssh-url корректно разбирает SSH URL."
  (let ((result (cl-git-tree/shell-utils:parse-ssh-url "user@example.com:/path/to/repo.git")))
    (is-true (equal result '("user" "example.com" "/path/to/repo.git")))))

(def-test parse-ssh-url-handles-no-path ()
  "Проверяем, что parse-ssh-url работает с URL без пути."
  (let ((result (cl-git-tree/shell-utils:parse-ssh-url "git@github.com:")))
    (is-true (equal result '("git" "github.com" "")))))

(def-test split-args-by-keys-separates-positional-and-keyword ()
  "Проверяем, что split-args-by-keys разделяет позиционные и ключевые аргументы."
  (let ((result (cl-git-tree/shell-utils:split-args-by-keys '("arg1" "arg2" "--key1" "val1" "--key2" "val2"))))
    (is-true (assoc :PREAMBLE result))
    (is-true (assoc :KEY1 result))
    (is-true (assoc :KEY2 result))))

(def-test split-args-by-keys-handles-no-keywords ()
  "Проверяем, что split-args-by-keys работает без ключевых аргументов."
  (let ((result (cl-git-tree/shell-utils:split-args-by-keys '("arg1" "arg2" "arg3"))))
    (is-true (equal (cdr (assoc :PREAMBLE result)) '("arg1" "arg2" "arg3")))))

(def-test split-args-by-keys-handles-all-keywords ()
  "Проверяем, что split-args-by-keys работает когда все аргументы - ключевые."
  (let ((result (cl-git-tree/shell-utils:split-args-by-keys '("--key1" "val1" "--key2" "val2"))))
    (is-true (assoc :KEY1 result))
    (is-true (assoc :KEY2 result))
    (is-true (not (assoc :ARGS result)))))
