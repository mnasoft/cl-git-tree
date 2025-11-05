;;;; /src/global/git-global.lisp

(in-package :cl-git-tree/global)

(defclass <git-global> ()
  ()
  (:documentation
   "Глобальная конфигурация Git (user.name, user.email и т.п.).
   Объект не хранит состояния, а служит точкой входа для вызова git config --global."))

(defparameter *git-global* (make-instance '<git-global>)
  "Единственный объект для работы с глобальной конфигурацией Git.")

(defparameter *git-global* (make-instance '<git-global>)
  "Единственный объект для работы с глобальной конфигурацией Git.")

;; --- Generic-функции ---

(defgeneric git-config-get (global key)
  (:documentation "Получить значение ключа KEY из глобальной конфигурации Git."))

(defgeneric git-config-set (global key value)
  (:documentation "Установить значение VALUE для ключа KEY в глобальной конфигурации Git."))

(defgeneric git-config-list (global)
  (:documentation "Вернуть список всех глобальных настроек Git."))

;; --- Методы ---

(defmethod git-config-get ((g <git-global>) key)
  (multiple-value-bind (out err code)
      (git-run "." "config" "--global" "--get" key)
    (declare (ignore err code))
    (string-trim '(#\Newline #\Space) out)))

(defmethod git-config-set ((g <git-global>) key value)
  (git-run "." "config" "--global" key value)
  value)

(defmethod git-config-list ((g <git-global>))
  (warn "g : ~S" g)
  (multiple-value-bind (out err code)
      (git-run "." "config" "--global" "--list")
    (declare (ignore err code))
    (split-sequence:split-sequence #\Newline out :remove-empty-subseqs t)))
