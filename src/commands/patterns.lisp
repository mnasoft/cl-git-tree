;;;; ./src/commands/patterns.lisp
;;;;
;;;; Команда для управления паттернами файлов.
;;;; Позволяет просматривать, добавлять, удалять паттерны.

(defpackage :cl-git-tree/commands/patterns
  (:use :cl
        :cl-git-tree/config)
  (:export cmd-patterns))

(in-package :cl-git-tree/commands/patterns)

(defun cmd-patterns-help ()
  "Показывает справку по команде patterns."
  (format t "Управление паттернами файлов.~%~%")
  (format t "Использование:~%")
  (format t "  git-tree patterns list [tracked|excluded]~%")
  (format t "  git-tree patterns add tracked <pattern>~%")
  (format t "  git-tree patterns add excluded <pattern>~%")
  (format t "  git-tree patterns remove tracked <pattern>~%")
  (format t "  git-tree patterns remove excluded <pattern>~%")
  (format t "  git-tree patterns reset~%~%")
  (format t "Примеры:~%")
  (format t "  git-tree patterns list           ;; показать все~%")
  (format t "  git-tree patterns list tracked   ;; только включения~%")
  (format t "  git-tree patterns add tracked *.rs~%")
  (format t "  git-tree patterns remove tracked *.tcl*~%")
  (format t "  git-tree patterns reset          ;; вернуть дефолты~%"))

(defun cmd-patterns-list (filter)
  "Показывает паттерны. FILTER может быть nil, 'tracked' или 'excluded'."
  (let ((ws (cl-git-tree/loc:make-workspace ".")))
    (cond
      ((null filter)
       ;; Показываем все
       (format t "~A Паттерны для ВКЛЮЧЕНИЯ:~%" (cl-git-tree/loc:find-emo ws "pin"))
       (dolist (p (get-tracked-patterns))
         (format t "  ~A ~A~%" (cl-git-tree/loc:find-emo ws "checked") p))
       (format t "~%~A Паттерны для ИСКЛЮЧЕНИЯ:~%" (cl-git-tree/loc:find-emo ws "block"))
       (dolist (p (get-excluded-patterns))
         (format t "  ~A ~A~%" (cl-git-tree/loc:find-emo ws "unchecked") p)))
      ((string= filter "tracked")
       (format t "~A Паттерны для ВКЛЮЧЕНИЯ:~%" (cl-git-tree/loc:find-emo ws "pin"))
       (dolist (p (get-tracked-patterns))
         (format t "  ~A ~A~%" (cl-git-tree/loc:find-emo ws "checked") p)))
      ((string= filter "excluded")
       (format t "~A Паттерны для ИСКЛЮЧЕНИЯ:~%" (cl-git-tree/loc:find-emo ws "block"))
       (dolist (p (get-excluded-patterns))
         (format t "  ~A ~A~%" (cl-git-tree/loc:find-emo ws "unchecked") p)))
      (t
       (format t "~A Неизвестный фильтр: ~A~%" (cl-git-tree/loc:find-emo ws "error") filter)
       (format t "Используйте: tracked или excluded~%")))))

(defun cmd-patterns-add (sub pattern)
  "Добавляет паттерн. SUB может быть 'tracked' или 'excluded'."
  (let ((ws (cl-git-tree/loc:make-workspace ".")))
    (if (null pattern)
        (format t "~A Укажите паттерн для добавления~%" (cl-git-tree/loc:find-emo ws "error"))
        (if (string= sub "tracked")
            (if (add-tracked-pattern pattern)
                (format t "~A Паттерн '~A' добавлен в включение~%" (cl-git-tree/loc:find-emo ws "success") pattern)
                (format t "~A  Паттерн '~A' уже есть в списке включения~%" (cl-git-tree/loc:find-emo ws "warning") pattern))
            (if (add-excluded-pattern pattern)
                (format t "~A Паттерн '~A' добавлен в исключение~%" (cl-git-tree/loc:find-emo ws "success") pattern)
                (format t "~A  Паттерн '~A' уже есть в списке исключения~%" (cl-git-tree/loc:find-emo ws "warning") pattern))))))

(defun cmd-patterns-remove (sub pattern)
  "Удаляет паттерн. SUB может быть 'tracked' или 'excluded'."
  (let ((ws (cl-git-tree/loc:make-workspace ".")))
    (if (null pattern)
        (format t "~A Укажите паттерн для удаления~%" (cl-git-tree/loc:find-emo ws "error"))
        (if (string= sub "tracked")
            (if (remove-tracked-pattern pattern)
                (format t "~A Паттерн '~A' удалён из включения~%" (cl-git-tree/loc:find-emo ws "success") pattern)
                (format t "~A  Паттерн '~A' не найден в списке включения~%" (cl-git-tree/loc:find-emo ws "warning") pattern))
            (if (remove-excluded-pattern pattern)
                (format t "~A Паттерн '~A' удалён из исключения~%" (cl-git-tree/loc:find-emo ws "success") pattern)
                (format t "~A  Паттерн '~A' не найден в списке исключения~%" (cl-git-tree/loc:find-emo ws "warning") pattern))))))

(defun cmd-patterns-reset ()
  "Сбрасывает паттерны на значения по умолчанию."
  (let ((ws (cl-git-tree/loc:make-workspace ".")))
    (reset-to-defaults)
    (format t "~A Паттерны сброшены на значения по умолчанию~%" (cl-git-tree/loc:find-emo ws "success"))))

(defun cmd-patterns (&rest args)
  "Управление паттернами файлов.
Команды:
  patterns list [tracked|excluded]  — показать все или конкретные паттерны
  patterns add tracked <pattern>    — добавить паттерн в список включения
  patterns add excluded <pattern>   — добавить паттерн в список исключения
  patterns remove tracked <pattern> — удалить паттерн из списка включения
  patterns remove excluded <pattern>— удалить паттерн из списка исключения
  patterns reset                    — сбросить на дефолты
  patterns --help                   — справка"
  (let ((cmd (first args))
        (sub (second args))
        (pattern (third args)))
    (cond
      ;; Справка
      ((or (null args) (member "--help" args :test #'string=))
       (cmd-patterns-help))
    
      ;; Просмотр паттернов
      ((string= cmd "list")
       (cmd-patterns-list sub))
    
      ;; Добавление паттерна: новый синтаксис `add tracked|excluded <pattern>`
      ((and (string= cmd "add")
            (member sub '("tracked" "excluded") :test #'string=))
       (cmd-patterns-add sub pattern))

      ;; Удаление паттерна: новый синтаксис `remove tracked|excluded <pattern>`
      ((and (string= cmd "remove")
            (member sub '("tracked" "excluded") :test #'string=))
       (cmd-patterns-remove sub pattern))
    
      ;; Сброс на дефолты
      ((string= cmd "reset")
       (cmd-patterns-reset))
    
      ;; Неизвестная команда
      (t
       (let ((ws (cl-git-tree/loc:make-workspace ".")))
         (format t "~A Неизвестная команда: ~A~%" (cl-git-tree/loc:find-emo ws "error") cmd)
         (format t "Используйте 'git-tree patterns --help' для справки~%"))))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "patterns" #'cmd-patterns "Управлять паттернами файлов"))
