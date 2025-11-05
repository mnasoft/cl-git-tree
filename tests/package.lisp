;;;; tests/package.lisp

(defpackage :cl-git-tree/tests
  (:use #:cl #:fiveam)
  (:export #:run-tests)
  (:documentation "
@b(Описание:) Пакет @b(math/tests) предназначен для проверки
изменений, внесенных в пакеты, связаные с :math.

Загузка пакета осуществляется через загрузку системы:

 @b(Пример использования:)
@begin[lang=lisp](code)
  (ql:quickload :math/tests)
@end(code)

Для выполнения всех тестов выполните:

 @b(Пример использования:)
@begin[lang=lisp](code)
  (run-tests)
@end(code)
"))

(in-package :cl-git-tree/tests)

(defun run-tests () (run! 'all))

