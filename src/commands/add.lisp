;;;; ./src/commands/add.lisp

(defpackage :cl-git-tree/commands/add
  (:use :cl
        :cl-git-tree/config)
  (:export cmd-add
           add-repo))

(in-package :cl-git-tree/commands/add)

(defun find-tracked-files (repo-dir
                           &key
                             (patterns *tracked-patterns*)
                             (excludes *excludes-patterns*))
  "@b(Назначение:) Ищет файлы по шаблонам @code(PATTERNS) внутри @code(REPO-DIR), пропуская @code(.git).  Возвращает список относительных путей.
@b(Пример:) @begin[lang=lisp](code)
 (find-tracked-files #P\"~/proj/repo/\"
   :patterns '(\"*.lisp\" \"*.asd\")
   :excludes '(\"./.git\" \"./build\"))
@end(code)"
  (let ((cwd
          (uiop:with-current-directory (repo-dir)
            (nth-value
             0
             (uiop:run-program
              (format nil "find . \\( ~{-path ~S ~^-o ~}\\) -prune -o -type f \\(~{ -name ~S ~^-o ~}\\) -print" excludes patterns)
              :output :string
              :error-output :string
              :ignore-error-status t
              :force-shell t)))))
    (remove-if #'uiop:emptyp
               (uiop:split-string cwd :separator '(#\Newline)))))

(defun add-repo (repo-dir args)
  "@b(Назначение:) Добавляет отслеживаемые файлы в индекс git через @code(repo-add).
@b(Пример:) @begin[lang=lisp](code)
 (add-repo \"/repos/demo\" '(:preamble \"*.lisp\"))
@end(code)"
  (let* ((wk (make-instance 'cl-git-tree/loc:<workspace> :path repo-dir))
         (alist    (cl-git-tree/shell-utils:split-args-by-keys args))
         (patterns (or (cdr (assoc :ARGS alist))
                       (cdr (assoc :PREAMBLE alist))))
         (dirs     (cdr (assoc :EXCLUDE alist))))
    ;; если паттерны не заданы явно, используем дефолтные
    (unless patterns (setf patterns *tracked-patterns*))
    ;; если исключения не заданы явно, используем дефолтные
    (unless dirs     (setf dirs *excludes-patterns*))
    ;; ищем файлы
    (let ((files (find-tracked-files (cl-git-tree/loc:git-root wk)
                                     :patterns patterns
                                     :excludes dirs)))
      (format t "~A ~A: найдено ~D файл(ов)~%" 
              (cl-git-tree/loc:find-emo wk "success")
              repo-dir 
              (length files))
      ;; вызываем метод repo-add
      (cl-git-tree/loc:repo-add wk :files files))))

(defun cmd-add (&rest args)
  "@b(CLI:) Добавляет отслеживаемые файлы во все git-репозитории.
@b(Пример:) @begin[lang=lisp](code)
 (cl-git-tree/cli:main '(\"prog\" \"add\" \"--\" \"*.lisp\" \"*.asd\"))
@end(code)"
  (let ((alist (cl-git-tree/shell-utils:split-args-by-keys args)))
    (cond
      ;; если указан ключ --help
      ((assoc :HELP alist)
       (format t "Добавляет все отслеживаемые изменения во всех git-репозиториях.~%~%")
       (format t "Использование:~%")
       (format t "  git-tree add [--exclude DIR ...] -- [PATTERNS...]~%")
       (format t "  git-tree add [PATTERNS...]~%~%")
       (format t "Аргументы:~%")
       (format t "  :EXCLUDE  → каталоги для исключения.~%")
       (format t "              По умолчанию: ~{~A ~}~%" *excludes-patterns*)
       (format t "  :ARGS     → шаблоны файлов для включения.~%")
       (format t "              По умолчанию: ~{~A ~}~%" *tracked-patterns*)
       (format t "~%Примеры:~%")
       (format t "  git-tree add --exclude ./tests ./build -- *.lisp *.asd~%")
       (format t "  git-tree add *.lisp *.asd~%")
       (format t "  git-tree add --   ;; использовать значения по умолчанию~%"))
      ;; иначе запускаем add-repo для каждого репозитория
      (t
       (cl-git-tree/fs:with-repo #'add-repo args)))))

;; регистрация команды при загрузке
(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "add" #'cmd-add "Добавить файлы в индекс"))
