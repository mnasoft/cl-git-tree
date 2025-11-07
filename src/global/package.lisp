(defpackage :cl-git-tree/global
  (:use :cl)
  (:export
   ;; класс
   <git-global>
   ;; generic-функции
   git-config-get
   git-config-set
   git-config-unset
   git-config-list
   ;; конструктор
   *git-global*
   ;; Хелперы
   git-get
   git-set
   git-unset
   git-list
   ))

(in-package :cl-git-tree/global)


