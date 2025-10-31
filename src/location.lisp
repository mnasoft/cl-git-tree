(in-package :cl-git-tree)

;;; ------------------------------
;;; Класс-шаблон для группы репозиториев
;;; ------------------------------

(defclass location-template ()
  ((name
    :initarg :name
    :accessor location-name
    :documentation "Человекочитаемое имя/идентификатор шаблона.")
   (url-git
    :initarg :url-git
    :accessor location-url-git
    :documentation "Базовый git-URL для группы репозиториев.")
   (url-xz
    :initarg :url-xz
    :accessor location-url-xz
    :initform nil
    :documentation "Путь к архиву .tar.xz, если используется.")
   (tar
    :initarg :tar
    :accessor location-tar
    :initform nil
    :documentation "Путь к tar-архиву, если используется.")
   (local
    :initarg
    :local
    :accessor location-local
    :initform nil
    :documentation
    "Флаг, указывающий, можно ли использовать эту локацию для локального клонирования.
Если NIL — решение принимается автоматически по url-git (локальный путь или UNC).
Если T — явно разрешено локальное клонирование, даже если URL выглядит сетевым.
Если :FORCE-NO — явно запрещено локальное клонирование.")))

;;; ------------------------------
;;; Глобальная таблица всех шаблонов
;;; ------------------------------

(defparameter *LOCATIONS* (make-hash-table :test 'equal))

;;; ------------------------------
;;; Утилиты для работы с локациями
;;; ------------------------------

(defun add-location (key &key name url-git url-xz tar)
  (when (gethash key *LOCATIONS*)
    (warn "Перезаписываю существующую локацию с ключом ~A" key))
  (setf (gethash key *LOCATIONS*)
        (make-instance 'location-template
                       :name name
                       :url-git url-git
                       :url-xz url-xz
                       :tar tar)))

(defun find-location (key)
  "Возвращает объект location-template по ключу или NIL."
  (gethash key *LOCATIONS*))

(defun location-exists-p (key)
  "Проверяет, существует ли шаблон с данным ключом."
  (not (null (find-location key))))

(defun all-location-keys ()
  "Возвращает список всех ключей локаций."
  (loop for k being the hash-keys of *LOCATIONS* collect k))

(defun all-locations ()
  "Возвращает список всех объектов location-template."
  (loop for v being the hash-values of *LOCATIONS* collect v))

;;; ------------------------------
;;; Пример вспомогательной функции
;;; ------------------------------

(defun repo-url (key repo-name)
  "Строит полный git-URL для конкретного репозитория внутри шаблона."
  (let ((loc (find-location key)))
    (when loc
      (concatenate 'string (location-url-git loc) repo-name ".git"))))


(defun print-locations ()
  "Выводит список всех шаблонов локаций в читаемом виде."
  (format t "~%=== Список локаций ===~%")
  (maphash (lambda (key loc)
             (format t "~A: ~A~%   Git: ~A~%   TAR: ~A~%   XZ : ~A~%~%"
                     key
                     (location-name loc)
                     (location-url-git loc)
                     (location-tar loc)
                     (location-url-xz loc)))
           *LOCATIONS*))

;;;
;;;
;;;

(defun infer-local-p (url)
  "Определяет, можно ли считать URL локальным."
  (or (uiop:string-prefix-p "/" url)     ;; абсолютный путь
      (uiop:string-prefix-p "//" url)))  ;; UNC / интрасеть

(defun location-local-p (loc)
  "Возвращает T, если location можно использовать для локального клонирования."
  (or (location-local loc)
      (infer-local-p (location-url-git loc))))
