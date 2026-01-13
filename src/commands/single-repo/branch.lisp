;;;; src/commands/single-repo/branch.lisp
;;;; CLI-команда для управления ветками git

(in-package :cl-git-tree/commands/single-repo)

(defun cmd-branch (args)
  "Команда git-tree branch - управление ветками git.

Использование:
  git-tree branch [опции] [аргументы]

Режимы работы:

1. Просмотр веток (по умолчанию):
   git-tree branch [--color[=<when>]] [--no-color] [--show-current]
                   [-v] [--abbrev[=<n>]] [--no-abbrev]
                   [--column[=<options>]] [--no-column]
                   [--sort=<key>] [--merged[=<commit>]] [--no-merged[=<commit>]]
                   [--contains[=<commit>]] [--no-contains[=<commit>]]
                   [--points-at=<object>] [--format=<format>]
                   [-r|--remotes] [-a|--all] [--list] [<pattern>...]

2. Создание ветки:
   git-tree branch [--track[=<mode>]] [--no-track] [-f]
                   [--recurse-submodules] <branch-name> [<start-point>]

3. Настройка upstream:
   git-tree branch --set-upstream-to=<upstream> [<branch-name>]
   git-tree branch -u <upstream> [<branch-name>]
   git-tree branch --unset-upstream [<branch-name>]

4. Переименование:
   git-tree branch -m [<old-branch>] <new-branch>
   git-tree branch -M [<old-branch>] <new-branch>

5. Копирование:
   git-tree branch -c [<old-branch>] <new-branch>
   git-tree branch -C [<old-branch>] <new-branch>

6. Удаление:
   git-tree branch -d [-r] <branch-name>...
   git-tree branch -D [-r] <branch-name>...

7. Редактирование описания:
   git-tree branch --edit-description [<branch-name>]

Примеры:
  git-tree branch                      # Список локальных веток
  git-tree branch -a                   # Все ветки (локальные и удаленные)
  git-tree branch -r                   # Только удаленные ветки
  git-tree branch feature              # Создать ветку feature
  git-tree branch -m old new           # Переименовать old в new
  git-tree branch -d feature           # Удалить ветку feature
  git-tree branch --merged main        # Ветки, слитые в main"
  
  (when (or (member "--help" args :test #'string=)
            (member "-h" args :test #'string=))
    (format t "~A~%" (documentation 'cmd-branch 'function))
    (return-from cmd-branch 0))
  
  (let ((color nil)
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
        (format-str nil)
        (remotes nil)
        (all nil)
        (list nil)
        (patterns '())
        (track nil)
        (no-track nil)
        (force nil)
        (recurse-submodules nil)
        (branch-name nil)
        (start-point nil)
        (set-upstream-to nil)
        (unset-upstream nil)
        (move nil)
        (move-force nil)
        (copy nil)
        (copy-force nil)
        (delete nil)
        (delete-force nil)
        (delete-remotes nil)
        (branch-names '())
        (edit-description nil)
        (old-branch nil)
        (new-branch nil)
        (mode nil)) ; :list, :create, :delete, :move, :copy, :upstream, :edit
    
    ;; Парсим аргументы
    (loop for arg in args do
      (cond
        ;; Опции просмотра
        ((string= arg "--color") (setf color t mode :list))
        ((cl-ppcre:scan "^--color=" arg)
         (setf color (subseq arg 8) mode :list))
        ((string= arg "--no-color") (setf no-color t mode :list))
        ((string= arg "--show-current") (setf show-current t mode :list))
        ((string= arg "-v") (setf verbose t mode :list))
        ((string= arg "--abbrev") (setf abbrev t mode :list))
        ((cl-ppcre:scan "^--abbrev=" arg)
         (setf abbrev (parse-integer (subseq arg 9)) mode :list))
        ((string= arg "--no-abbrev") (setf no-abbrev t mode :list))
        ((string= arg "--column") (setf column t mode :list))
        ((cl-ppcre:scan "^--column=" arg)
         (setf column (subseq arg 9) mode :list))
        ((string= arg "--no-column") (setf no-column t mode :list))
        ((cl-ppcre:scan "^--sort=" arg)
         (setf sort (subseq arg 7) mode :list))
        ((string= arg "--merged") (setf merged t mode :list))
        ((cl-ppcre:scan "^--merged=" arg)
         (setf merged (subseq arg 9) mode :list))
        ((string= arg "--no-merged") (setf no-merged t mode :list))
        ((cl-ppcre:scan "^--no-merged=" arg)
         (setf no-merged (subseq arg 12) mode :list))
        ((string= arg "--contains") (setf contains t mode :list))
        ((cl-ppcre:scan "^--contains=" arg)
         (setf contains (subseq arg 11) mode :list))
        ((string= arg "--no-contains") (setf no-contains t mode :list))
        ((cl-ppcre:scan "^--no-contains=" arg)
         (setf no-contains (subseq arg 14) mode :list))
        ((cl-ppcre:scan "^--points-at=" arg)
         (setf points-at (subseq arg 12) mode :list))
        ((cl-ppcre:scan "^--format=" arg)
         (setf format-str (subseq arg 9) mode :list))
        ((or (string= arg "-r") (string= arg "--remotes"))
         (if (eq mode :delete)
             (setf delete-remotes t)
             (setf remotes t mode :list)))
        ((or (string= arg "-a") (string= arg "--all"))
         (setf all t mode :list))
        ((string= arg "--list") (setf list t mode :list))
        
        ;; Опции создания
        ((string= arg "--track") (setf track t mode :create))
        ((cl-ppcre:scan "^--track=" arg)
         (setf track (subseq arg 8) mode :create))
        ((string= arg "--no-track") (setf no-track t mode :create))
        ((string= arg "-f")
         (if (eq mode :create)
             (setf force t)
             (setf force t mode :create)))
        ((string= arg "--recurse-submodules")
         (setf recurse-submodules t mode :create))
        
        ;; Опции upstream
        ((cl-ppcre:scan "^--set-upstream-to=" arg)
         (setf set-upstream-to (subseq arg 19) mode :upstream))
        ((string= arg "-u")
         (setf set-upstream-to (pop args) mode :upstream))
        ((string= arg "--unset-upstream")
         (setf unset-upstream t mode :upstream))
        
        ;; Опции переименования
        ((string= arg "-m") (setf move t mode :move))
        ((string= arg "-M") (setf move-force t mode :move))
        
        ;; Опции копирования
        ((string= arg "-c") (setf copy t mode :copy))
        ((string= arg "-C") (setf copy-force t mode :copy))
        
        ;; Опции удаления
        ((string= arg "-d") (setf delete t mode :delete))
        ((string= arg "-D") (setf delete-force t mode :delete))
        
        ;; Редактирование описания
        ((string= arg "--edit-description")
         (setf edit-description t mode :edit))
        
        ;; Позиционные аргументы
        (t
         (cond
           ((eq mode :delete)
            (push arg branch-names))
           ((eq mode :move)
            (if old-branch
                (setf new-branch arg)
                (if new-branch
                    (progn
                      (setf old-branch new-branch)
                      (setf new-branch arg))
                    (setf new-branch arg))))
           ((eq mode :copy)
            (if old-branch
                (setf new-branch arg)
                (if new-branch
                    (progn
                      (setf old-branch new-branch)
                      (setf new-branch arg))
                    (setf new-branch arg))))
           ((or (eq mode :create) (eq mode nil))
            (if branch-name
                (setf start-point arg mode :create)
                (setf branch-name arg mode :create)))
           ((eq mode :upstream)
            (setf branch-name arg))
           ((eq mode :edit)
            (setf branch-name arg))
           (t
            (push arg patterns))))))
    
    (setf patterns (nreverse patterns))
    (setf branch-names (nreverse branch-names))
    
    ;; Получаем текущий workspace
    (let ((ws (cl-git-tree/loc:current-workspace-or-die)))
      (multiple-value-bind (out code)
          (cl-git-tree/loc:repo-branch
           ws
           :color color
           :no-color no-color
           :show-current show-current
           :verbose verbose
           :abbrev abbrev
           :no-abbrev no-abbrev
           :column column
           :no-column no-column
           :sort sort
           :merged merged
           :no-merged no-merged
           :contains contains
           :no-contains no-contains
           :points-at points-at
           :format format-str
           :remotes remotes
           :all all
           :list list
           :patterns patterns
           :track track
           :no-track no-track
           :force force
           :recurse-submodules recurse-submodules
           :branch-name branch-name
           :start-point start-point
           :set-upstream-to set-upstream-to
           :unset-upstream unset-upstream
           :move move
           :move-force move-force
           :old-branch old-branch
           :new-branch new-branch
           :copy copy
           :copy-force copy-force
           :delete delete
           :delete-force delete-force
           :delete-remotes delete-remotes
           :branch-names branch-names
           :edit-description edit-description)
        (declare (ignore out))
        code))))

;; Регистрация команды
(cl-git-tree/dispatch:register-command
 "branch"
 #'cmd-branch
 "Управление ветками git (просмотр, создание, удаление, переименование)")
