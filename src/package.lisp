;;;; ./src/package.lisp

(defpackage :cl-git-tree
  (:use :cl)
  (:documentation "@b(Пакет:) Точка входа cl-git-tree; загрузка и сброс конфигурации.

@b(Пример:)
@begin[lang=lisp](code)
 (cl-git-tree:load-config)
 (cl-git-tree:reset-config)
@end(code)")
  (:export 
           load-config
           reset-config
           *config-path*))

(in-package :cl-git-tree)

