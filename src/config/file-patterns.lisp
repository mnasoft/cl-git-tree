;;;; ./src/config/file-patterns.lisp
;;;;
;;;; Управление паттернами файлов для включения/исключения.
;;;; Паттерны сохраняются в ~/.git-tree/file-patterns.lisp

(in-package :cl-git-tree/config)

(defparameter *patterns-path*
  (merge-pathnames #p"file-patterns.lisp"
                   (merge-pathnames #p".git-tree/"
                                    (user-homedir-pathname)))
  "Путь к файлу с пользовательскими паттернами файлов.
   Файл хранится в ~/.git-tree/file-patterns.lisp")

(defclass <file-patterns> ()
  ((tracked :initarg :tracked
            :initform '("*.lisp" "*.org" "*.asd" "*.c*" "*.h*" "*.tcl*" ".gitignore")
            :accessor tracked-patterns
            :documentation "Паттерны для включения файлов")
   (excluded :initarg :excluded
             :initform '("./.git" "./build")
             :accessor excluded-patterns
             :documentation "Паттерны для исключения файлов"))
  (:documentation
   "Объект для управления паттернами файлов.
    Сохраняет списки паттернов для включения и исключения файлов."))

(defparameter *file-patterns*
  (make-instance '<file-patterns>)
  "Глобальный объект паттернов файлов.")

;; --- Загрузка/сохранение ---

(defun load-patterns ()
  "Загружает паттерны из ~/.git-tree/file-patterns.lisp или создаёт файл с дефолтами."
  (if (probe-file *patterns-path*)
      (with-open-file (s *patterns-path* :direction :input)
        (let ((data (read s)))
          (setf (tracked-patterns *file-patterns*) (getf data :tracked))
          (setf (excluded-patterns *file-patterns*) (getf data :excluded))))
      ;; Если файла нет, используем дефолты и сохраняем
      (progn
        (save-patterns)
        nil)))

(defun save-patterns ()
  "Сохраняет текущие паттерны в ~/.git-tree/file-patterns.lisp"
  (ensure-directories-exist
   (merge-pathnames #p".git-tree/" (user-homedir-pathname)))
  (with-open-file (s *patterns-path*
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
    (let ((data (list :tracked (tracked-patterns *file-patterns*)
                      :excluded (excluded-patterns *file-patterns*))))
      (format s "~S~%" data))))

;; --- Чтение паттернов ---

(defun get-tracked-patterns ()
  "Возвращает список паттернов для включения файлов."
  (copy-list (tracked-patterns *file-patterns*)))

(defun get-excluded-patterns ()
  "Возвращает список паттернов для исключения файлов."
  (copy-list (excluded-patterns *file-patterns*)))

(defun list-all-patterns ()
  "Возвращает ассоциативный список ((:tracked . PATTERNS) (:excluded . PATTERNS))"
  (list (cons :tracked (get-tracked-patterns))
        (cons :excluded (get-excluded-patterns))))

;; --- Добавление паттернов ---

(defun add-tracked-pattern (pattern)
  "Добавляет паттерн в список включения (если его там ещё нет).
   Возвращает T если добавлен, NIL если уже был."
  (unless (member pattern (tracked-patterns *file-patterns*) :test #'string=)
    (push pattern (tracked-patterns *file-patterns*))
    (save-patterns)
    t))

(defun add-excluded-pattern (pattern)
  "Добавляет паттерн в список исключения (если его там ещё нет).
   Возвращает T если добавлен, NIL если уже был."
  (unless (member pattern (excluded-patterns *file-patterns*) :test #'string=)
    (push pattern (excluded-patterns *file-patterns*))
    (save-patterns)
    t))

;; --- Удаление паттернов ---

(defun remove-tracked-pattern (pattern)
  "Удаляет паттерн из списка включения.
   Возвращает T если удалён, NIL если его не было."
  (let ((old-len (length (tracked-patterns *file-patterns*))))
    (setf (tracked-patterns *file-patterns*)
          (remove pattern (tracked-patterns *file-patterns*) :test #'string=))
    (when (< (length (tracked-patterns *file-patterns*)) old-len)
      (save-patterns)
      t)))

(defun remove-excluded-pattern (pattern)
  "Удаляет паттерн из списка исключения.
   Возвращает T если удалён, NIL если его не было."
  (let ((old-len (length (excluded-patterns *file-patterns*))))
    (setf (excluded-patterns *file-patterns*)
          (remove pattern (excluded-patterns *file-patterns*) :test #'string=))
    (when (< (length (excluded-patterns *file-patterns*)) old-len)
      (save-patterns)
      t)))

;; --- Сброс на дефолты ---

(defun reset-to-defaults ()
  "Сбрасывает все паттерны на значения по умолчанию и сохраняет."
  (setf (tracked-patterns *file-patterns*)
        '("*.lisp" "*.org" "*.asd" "*.c*" "*.h*" "*.tcl*" ".gitignore"))
  (setf (excluded-patterns *file-patterns*)
        '("./.git" "./build"))
  (save-patterns)
  t)

;; --- Инициализация при загрузке модуля ---

(eval-when (:load-toplevel :execute)
  (load-patterns))
