;;;; /src/global/git-global.lisp

(in-package :cl-git-tree/global)

(defclass <git-global> ()
  ()
  (:documentation
   "Глобальная конфигурация Git (user.name, user.email и т.п.).
   Объект не хранит состояния, а служит точкой входа для вызова git config --global."))

(defparameter *git-global* (make-instance '<git-global>)
  "Единственный объект для работы с глобальной конфигурацией Git.")

;; --- Generic-функции ---

(defgeneric git-config-get (global key)
  (:documentation "Получить значение ключа KEY из глобальной конфигурации Git."))

(defgeneric git-config-set (global key value)
  (:documentation "Установить значение VALUE для ключа KEY в глобальной конфигурации Git."))

(defgeneric git-config-list (global)
  (:documentation "Вернуть список всех глобальных настроек Git."))

(defgeneric git-config-unset (global key)
  (:documentation "Удалить значение для ключа KEY из глобальной конфигурации Git."))

;; --- Методы ---

(defmethod git-config-get ((g <git-global>) key)
  (multiple-value-bind (out err code)
      (cl-git-tree/git-utils:git-run "." "config" "--global" "--get" key)
    (declare (ignore err code))
    (string-trim '(#\Newline #\Space) out)))

(defmethod git-config-set ((g <git-global>) key value)
  (cl-git-tree/git-utils:git-run "." "config" "--global" key value)
  value)

(defmethod git-config-list ((g <git-global>))
  (multiple-value-bind (out err code)
      (cl-git-tree/git-utils:git-run "." "config" "--global" "--list")
    (declare (ignore err code))
    (split-sequence:split-sequence #\Newline out :remove-empty-subseqs t)))

(defmethod git-config-unset ((g <git-global>) key)
  (multiple-value-bind (out err code)
      (cl-git-tree/git-utils:git-run "." "config" "--global" "--unset" key)
    (declare (ignore out err))
    (= code 0)))

;; --- Хелперы для глобальной конфигурации ---

(defun git-get (key)
  "Укороченный вызов git-config-get для *git-global*."
  (git-config-get *git-global* key))

(defun git-set (key value)
  "Укороченный вызов git-config-set для *git-global*."
  (git-config-set *git-global* key value))

(defun git-unset (key)
  "Укороченный вызов git-config-unset для *git-global*."
  (git-config-unset *git-global* key))

(defun git-list ()
  "Укороченный вызов git-config-list для *git-global*."
  (git-config-list *git-global*))
