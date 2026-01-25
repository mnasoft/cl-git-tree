;;;; /src/global/git-global.lisp

(in-package :cl-git-tree/global)

(defclass <git-global> ()
  ()
  (:documentation
   "@b(Назначение:) Представляет глобальную конфигурацию Git (user.name, user.email и др.), используя @code(git config --global).
@b(Пример:) @begin[lang=lisp](code)
 (git-set \"user.name\" \"Alice\")
 (git-get \"user.name\")
@end(code)"))

(defparameter *git-global* (make-instance '<git-global>)
  "@b(Назначение:) Единственный объект для доступа к глобальной конфигурации Git.")

;; --- Generic-функции ---

(defgeneric git-config-get (global key)
  (:documentation "@b(Назначение:) Получить значение ключа @code(key) из глобальной конфигурации Git.
@b(Пример:) @begin[lang=lisp](code)
 (git-config-get *git-global* \"user.email\")
@end(code)"))

(defgeneric git-config-set (global key value)
  (:documentation "@b(Назначение:) Установить значение @code(value) для ключа @code(key) в глобальной конфигурации Git.
@b(Пример:) @begin[lang=lisp](code)
 (git-config-set *git-global* \"user.name\" \"Alice Example\")
@end(code)"))

(defgeneric git-config-list (global)
  (:documentation "@b(Назначение:) Вернуть список всех глобальных настроек Git.
@b(Пример:) @begin[lang=lisp](code)
 (git-config-list *git-global*)
@end(code)"))

(defgeneric git-config-unset (global key)
  (:documentation "@b(Назначение:) Удалить значение ключа @code(key) из глобальной конфигурации Git.
@b(Пример:) @begin[lang=lisp](code)
 (git-config-unset *git-global* \"user.signingkey\")
@end(code)"))

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
  "@b(Назначение:) Сокращённый вызов @code(git-config-get) для @code(*git-global*).
@b(Пример:) @begin[lang=lisp](code)
 (git-get \"user.email\")
@end(code)"
  (git-config-get *git-global* key))

(defun git-set (key value)
  "@b(Назначение:) Сокращённый вызов @code(git-config-set) для @code(*git-global*).
@b(Пример:) @begin[lang=lisp](code)
 (git-set \"user.name\" \"Alice Example\")
@end(code)"
  (git-config-set *git-global* key value))

(defun git-unset (key)
  "@b(Назначение:) Сокращённый вызов @code(git-config-unset) для @code(*git-global*).
@b(Пример:) @begin[lang=lisp](code)
 (git-unset \"user.signingkey\")
@end(code)"
  (git-config-unset *git-global* key))

(defun git-list ()
  "@b(Назначение:) Сокращённый вызов @code(git-config-list) для @code(*git-global*).
@b(Пример:) @begin[lang=lisp](code)
 (git-list)
@end(code)"
  (git-config-list *git-global*))
