;;;; ./src/commands/patterns.lisp
;;;;
;;;; Команда для управления паттернами файлов.
;;;; Позволяет просматривать, добавлять, удалять паттерны.

(defpackage :cl-git-tree/commands/patterns
  (:use :cl
        :cl-git-tree/config)
  (:export cmd-patterns))

(in-package :cl-git-tree/commands/patterns)

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
    
    ;; Просмотр паттернов
    ((string= cmd "list")
     (let ((filter (second args)))
       (cond
         ((null filter)
          ;; Показываем все
          (format t "📌 Паттерны для ВКЛЮЧЕНИЯ:~%")
          (dolist (p (get-tracked-patterns))
            (format t "  ✓ ~A~%" p))
          (format t "~%🚫 Паттерны для ИСКЛЮЧЕНИЯ:~%")
          (dolist (p (get-excluded-patterns))
            (format t "  ✗ ~A~%" p)))
         ((string= filter "tracked")
          (format t "📌 Паттерны для ВКЛЮЧЕНИЯ:~%")
          (dolist (p (get-tracked-patterns))
            (format t "  ✓ ~A~%" p)))
         ((string= filter "excluded")
          (format t "🚫 Паттерны для ИСКЛЮЧЕНИЯ:~%")
          (dolist (p (get-excluded-patterns))
            (format t "  ✗ ~A~%" p)))
         (t
          (format t "❌ Неизвестный фильтр: ~A~%" filter)
          (format t "Используйте: tracked или excluded~%")))))
    
    ;; Добавление паттерна: новый синтаксис `add tracked|excluded <pattern>`
    ((and (string= cmd "add")
        (member sub '("tracked" "excluded") :test #'string=))
     (if (null pattern)
       (format t "❌ Укажите паттерн для добавления~%")
       (if (string= sub "tracked")
         (if (add-tracked-pattern pattern)
           (format t "✅ Паттерн '~A' добавлен в включение~%" pattern)
           (format t "⚠️  Паттерн '~A' уже есть в списке включения~%" pattern))
         (if (add-excluded-pattern pattern)
           (format t "✅ Паттерн '~A' добавлен в исключение~%" pattern)
           (format t "⚠️  Паттерн '~A' уже есть в списке исключения~%" pattern)))))

    ;; Удаление паттерна: новый синтаксис `remove tracked|excluded <pattern>`
    ((and (string= cmd "remove")
        (member sub '("tracked" "excluded") :test #'string=))
     (if (null pattern)
       (format t "❌ Укажите паттерн для удаления~%")
       (if (string= sub "tracked")
         (if (remove-tracked-pattern pattern)
           (format t "✅ Паттерн '~A' удалён из включения~%" pattern)
           (format t "⚠️  Паттерн '~A' не найден в списке включения~%" pattern))
         (if (remove-excluded-pattern pattern)
           (format t "✅ Паттерн '~A' удалён из исключения~%" pattern)
           (format t "⚠️  Паттерн '~A' не найден в списке исключения~%" pattern)))))

    ;; Обратная совместимость: старые односоставные подкоманды
    ((string= cmd "add-tracked")
     (if (null (second args))
       (format t "❌ Укажите паттерн для добавления~%")
       (let ((p (second args)))
         (if (add-tracked-pattern p)
           (format t "✅ Паттерн '~A' добавлен в включение (add-tracked, устарело; используйте add tracked)~%" p)
           (format t "⚠️  Паттерн '~A' уже есть в списке включения~%" p)))))

    ((string= cmd "add-excluded")
     (if (null (second args))
       (format t "❌ Укажите паттерн для добавления~%")
       (let ((p (second args)))
         (if (add-excluded-pattern p)
           (format t "✅ Паттерн '~A' добавлен в исключение (add-excluded, устарело; используйте add excluded)~%" p)
           (format t "⚠️  Паттерн '~A' уже есть в списке исключения~%" p)))))

    ((string= cmd "remove-tracked")
     (if (null (second args))
       (format t "❌ Укажите паттерн для удаления~%")
       (let ((p (second args)))
         (if (remove-tracked-pattern p)
           (format t "✅ Паттерн '~A' удалён из включения (remove-tracked, устарело; используйте remove tracked)~%" p)
           (format t "⚠️  Паттерн '~A' не найден в списке включения~%" p)))))

    ((string= cmd "remove-excluded")
     (if (null (second args))
       (format t "❌ Укажите паттерн для удаления~%")
       (let ((p (second args)))
         (if (remove-excluded-pattern p)
           (format t "✅ Паттерн '~A' удалён из исключения (remove-excluded, устарело; используйте remove excluded)~%" p)
           (format t "⚠️  Паттерн '~A' не найден в списке исключения~%" p)))))
    
    ;; Сброс на дефолты
    ((string= cmd "reset")
     (reset-to-defaults)
     (format t "✅ Паттерны сброшены на значения по умолчанию~%"))
    
    ;; Неизвестная команда
    (t
     (format t "❌ Неизвестная команда: ~A~%" cmd)
     (format t "Используйте 'git-tree patterns --help' для справки~%")))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "patterns" #'cmd-patterns "Управлять паттернами файлов"))
