;;;; ./src/emodji/package.lisp

(defpackage :cl-git-tree/emodji
  (:use :cl)
  (:export <emodji>
           <emodji>-mono
           <emodji>-name
           <emodji>-color
           <emodji>-category
           <emodji>-description)
  (:export *emodji-table*
           find-emodji
           define-emodji))

(in-package :cl-git-tree/emodji)
