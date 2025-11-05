(defpackage :cl-git-tree/global
  (:use :cl :cl-git-tree/git-utils)
  (:export
   ;; класс
   <git-global>
   ;; generic-функции
   git-config-get
   git-config-set
   git-config-list
   ;; конструктор
   *git-global*))

(in-package :cl-git-tree/global)
