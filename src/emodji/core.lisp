;;;; ./src/emodji/core.lisp

(in-package :cl-git-tree/emodji)

(defclass <emodji> ()
  ((name        :initarg :name        :accessor <emodji>-name)
   (description :initarg :description :accessor <emodji>-description)
   (category    :initarg :category    :accessor <emodji>-category)
   (mono        :initarg :mono        :accessor <emodji>-mono)
   (color       :initarg :color       :accessor <emodji>-color)))

(defparameter *emodji-table*
  (make-hash-table :test #'equal))

(defmacro define-emodji (key category description mono color)
  `(setf (gethash ,key *emodji-table*)
         (make-instance '<emodji>
                        :name         ,key
                        :category     ,category
                        :description  ,description
                        :mono         ,mono
                        :color        ,color)))

(defun find-emodji (key)
  (gethash key *emodji-table*))
