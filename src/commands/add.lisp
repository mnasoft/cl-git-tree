;;;; ./src/commands/add.lisp

(defpackage :cl-git-tree/commands/add
  (:use :cl)
  (:export cmd-add
           add-repo))

(in-package :cl-git-tree/commands/add)

(defparameter *tracked-patterns*
  '("*.lisp" "*.org" "*.asd" "*.c*" "*.h*" "*.tcl*" ".gitignore")
  "Список шаблонов файлов, которые автоматически добавляются в индекс.")

(defparameter *excludes-patterns*
  '("./.git" "./build")
  "Список шаблонов файлов, которые автоматически добавляются в индекс.")

(defun find-tracked-files (repo-dir
                           &key
                             (patterns *tracked-patterns*)
                             (excludes *excludes-patterns*))
  "Ищет файлы по шаблонам PATTERNS внутри REPO-DIR, исключая .git.
   Команда формируется как строка."
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

#+nil
(defun add-repo (repo-dir args)
  "Добавляет отслеживаемые файлы в git-индекс." 
  (declare (ignore args))
  (let ((files (find-tracked-files repo-dir)))
    (format t "✔ ~A: найдено ~D файл(ов)~%" repo-dir (length files))
    (multiple-value-bind (_out err code)
        (apply #'cl-git-tree/git-utils:git-run repo-dir "add" files)
      (declare (ignore _out))
      (if (zerop code)
          (format t "✔ ~A: файлы добавлены~%" repo-dir)
          (format t "❌ ~A: ошибка при git add:~%~A" repo-dir err)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun split-args-by-keys (args)
  "Разделяет сптск аргументов на alist вида ((:key (values ...)) ...).
  Ключи должны начинаться с \"--\"."
  (let ((result '())
        (current-key "--preamble")
        (current-values '()))
    (labels ((flush ()
               (when current-key
                 (push (cons
                        (intern
                         (string-upcase (subseq current-key 2)) :keyword)
                             (nreverse current-values))
                       result))
               (setf current-values '())))
      (dolist (arg args)
        (if (and (>= (length arg) 2)
                 (string= (subseq arg 0 2) "--"))
            (progn
              (flush)
              (setf current-key arg))
            (push arg current-values)))
      (flush))
    (nreverse result)))

(defun add-repo (repo-dir args)
  (let ((wk (make-instance 'cl-git-tree/loc:<workspace> :path repo-dir))
        (alist (split-args-by-keys args)))
    (format t "~S~%" alist)
    (format t "~S~%" (cdr (assoc :PREAMBLE alist)))
    #+nil
    (find-tracked-files (cl-git-tree/loc:git-root wk) 
         :patterns args)
    ))

#+nil
(format t "~S"
        (find-tracked-files
         (cl-git-tree/loc:git-root wk)
         :patterns args))

#+nil
(split-args-by-keys '("assa" "--files" "*.lisp" "*.asd" "--dirs" "./.git"))

#+nil 
(
  (let ((files (find-tracked-files repo-dir)))
    (format t "✔ ~A: найдено ~D файл(ов)~%" repo-dir (length files))
    (multiple-value-bind (_out err code)
        (apply #'cl-git-tree/git-utils:git-run repo-dir "add" files)
      (declare (ignore _out))
      (if (zerop code)
          (format t "✔ ~A: файлы добавлены~%" repo-dir)
          (format t "❌ ~A: ошибка при git add:~%~A" repo-dir err)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmd-add (&rest args)
  "CLI-команда: добавить отслеживаемые файлы во все git-репозитории."
  (cond
    ((member "--help" args :test #'string=)
     (format t "Добавляет все отслеживаемые изменения во всех git-репозиториях.~%~%")
     (format t "Использование:~%  git-tree add~%")
     (format t "Пример:~%  git-tree add~%"))
    (t
     (cl-git-tree/fs:with-repo #'add-repo args))))

;; регистрация команды при загрузке
(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "add" #'cmd-add "Добавить файлы в индекс"))
