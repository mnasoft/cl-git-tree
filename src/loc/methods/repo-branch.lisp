;;;; src/loc/methods/repo-branch.lisp
;;;; Метод для управления ветками git (list, create, delete, rename, copy)

(in-package :cl-git-tree/loc)

(defmethod repo-branch ((ws <workspace>)
                        &key
                        ;; Опции просмотра
                        (color nil)
                        (no-color nil)
                        (show-current nil)
                        (verbose nil)
                        (abbrev nil)
                        (no-abbrev nil)
                        (column nil)
                        (no-column nil)
                        (sort nil)
                        (merged nil)
                        (no-merged nil)
                        (contains nil)
                        (no-contains nil)
                        (points-at nil)
                        (format nil)
                        (remotes nil)
                        (all nil)
                        (list nil)
                        (patterns nil)
                        ;; Опции создания ветки
                        (track nil)
                        (no-track nil)
                        (force nil)
                        (recurse-submodules nil)
                        (branch-name nil)
                        (start-point nil)
                        ;; Опции upstream
                        (set-upstream-to nil)
                        (unset-upstream nil)
                        ;; Опции переименования
                        (move nil)
                        (move-force nil)
                        (old-branch nil)
                        (new-branch nil)
                        ;; Опции копирования
                        (copy nil)
                        (copy-force nil)
                        ;; Опции удаления
                        (delete nil)
                        (delete-force nil)
                        (delete-remotes nil)
                        (branch-names nil)
                        ;; Редактирование описания
                        (edit-description nil))
  "Выполнить операции с ветками git в указанном workspace.

Режимы работы:
1. Просмотр веток (по умолчанию):
   :color - цветной вывод (true/false/always/never/auto)
   :no-color - отключить цвет
   :show-current - показать только текущую ветку
   :verbose - подробный вывод с хешами и описаниями коммитов
   :abbrev - длина сокращенного хеша (число)
   :no-abbrev - не сокращать хеши
   :column - вывод в колонках (опции)
   :no-column - отключить вывод в колонках
   :sort - ключ сортировки (refname, objecttype, etc)
   :merged - только ветки, слитые в указанный коммит
   :no-merged - только ветки, не слитые в указанный коммит
   :contains - только ветки, содержащие указанный коммит
   :no-contains - только ветки, не содержащие указанный коммит
   :points-at - только ветки, указывающие на объект
   :format - формат вывода
   :remotes - показать удаленные ветки
   :all - показать все ветки (локальные и удаленные)
   :list - режим списка (по умолчанию)
   :patterns - список шаблонов для фильтрации

2. Создание ветки:
   :branch-name - имя новой ветки
   :start-point - начальная точка (коммит/ветка)
   :track - настроить отслеживание (direct/inherit)
   :no-track - не настраивать отслеживание
   :force - создать принудительно (перезаписать существующую)
   :recurse-submodules - обновить подмодули

3. Настройка upstream:
   :set-upstream-to - установить upstream для ветки
   :unset-upstream - удалить upstream для ветки

4. Переименование:
   :move - переименовать ветку (-m)
   :move-force - переименовать принудительно (-M)
   :old-branch - старое имя (необязательно, по умолчанию текущая)
   :new-branch - новое имя

5. Копирование:
   :copy - скопировать ветку (-c)
   :copy-force - скопировать принудительно (-C)
   :old-branch - исходная ветка
   :new-branch - новое имя

6. Удаление:
   :delete - удалить ветку (-d)
   :delete-force - удалить принудительно (-D)
   :delete-remotes - удалить удаленную ветку (-r)
   :branch-names - список имен веток для удаления

7. Редактирование описания:
   :edit-description - редактировать описание ветки

Возвращает (values output exit-code)."
  (let ((args '())
        (emo-name "git branch")
        (emo-success (find-emo "git branch" :success t))
        (emo-failure (find-emo "git branch" :failure t)))
    
    ;; Определяем режим работы и строим аргументы
    (cond
      ;; Режим удаления
      ((or delete delete-force)
       (when delete (push "-d" args))
       (when delete-force (push "-D" args))
       (when delete-remotes (push "-r" args))
       (when branch-names
         (dolist (name (if (listp branch-names) branch-names (list branch-names)))
           (push name args))))
      
      ;; Режим переименования
      ((or move move-force)
       (when move (push "-m" args))
       (when move-force (push "-M" args))
       (when old-branch (push old-branch args))
       (when new-branch (push new-branch args)))
      
      ;; Режим копирования
      ((or copy copy-force)
       (when copy (push "-c" args))
       (when copy-force (push "-C" args))
       (when old-branch (push old-branch args))
       (when new-branch (push new-branch args)))
      
      ;; Режим редактирования описания
      (edit-description
       (push "--edit-description" args)
       (when branch-name (push branch-name args)))
      
      ;; Режим установки upstream
      (set-upstream-to
       (push (format nil "--set-upstream-to=~A" set-upstream-to) args)
       (when branch-name (push branch-name args)))
      
      ;; Режим удаления upstream
      (unset-upstream
       (push "--unset-upstream" args)
       (when branch-name (push branch-name args)))
      
      ;; Режим создания ветки
      (branch-name
       (when track
         (if (stringp track)
             (push (format nil "--track=~A" track) args)
             (push "--track" args)))
       (when no-track (push "--no-track" args))
       (when force (push "-f" args))
       (when recurse-submodules (push "--recurse-submodules" args))
       (push branch-name args)
       (when start-point (push start-point args)))
      
      ;; Режим просмотра (по умолчанию)
      (t
       (when color
         (if (stringp color)
             (push (format nil "--color=~A" color) args)
             (push "--color" args)))
       (when no-color (push "--no-color" args))
       (when show-current (push "--show-current" args))
       (when verbose (push "-v" args))
       (when abbrev
         (if (numberp abbrev)
             (push (format nil "--abbrev=~D" abbrev) args)
             (push "--abbrev" args)))
       (when no-abbrev (push "--no-abbrev" args))
       (when column
         (if (stringp column)
             (push (format nil "--column=~A" column) args)
             (push "--column" args)))
       (when no-column (push "--no-column" args))
       (when sort (push (format nil "--sort=~A" sort) args))
       (when merged
         (if (stringp merged)
             (push (format nil "--merged=~A" merged) args)
             (push "--merged" args)))
       (when no-merged
         (if (stringp no-merged)
             (push (format nil "--no-merged=~A" no-merged) args)
             (push "--no-merged" args)))
       (when contains
         (if (stringp contains)
             (push (format nil "--contains=~A" contains) args)
             (push "--contains" args)))
       (when no-contains
         (if (stringp no-contains)
             (push (format nil "--no-contains=~A" no-contains) args)
             (push "--no-contains" args)))
       (when points-at (push (format nil "--points-at=~A" points-at) args))
       (when format (push (format nil "--format=~A" format) args))
       (when remotes (push "-r" args))
       (when all (push "-a" args))
       (when list (push "--list" args))
       (when patterns
         (dolist (pattern (if (listp patterns) patterns (list patterns)))
           (push pattern args)))))
    
    (setf args (nreverse args))
    
    ;; Выполняем команду
    (multiple-value-bind (out code)
        (apply #'cl-git-tree/git-utils:git-run
               "branch"
               :dir (<workspace>-path ws)
               args)
      (if (zerop code)
          (progn
            (format t "~A ~A~%" emo-success emo-name)
            (when out (format t "~A~%" out)))
          (format t "~A ~A (код выхода: ~D)~%~A~%" emo-failure emo-name code out))
      (values out code))))
