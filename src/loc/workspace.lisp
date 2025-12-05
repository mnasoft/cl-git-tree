;;;; ./src/workspace/workspace.lisp

(in-package :cl-git-tree/loc)

(defun make-workspace (path &key description)
  "Создать объект <workspace> для указанного каталога PATH.
   Аргумент DESCRIPTION задаёт человекочитаемое описание.
   Если DESCRIPTION не указан, он вычисляется как имя каталога
   (последний компонент PATH).
   PATH всегда трактуется как каталог, даже если он указан без завершающего /."
  (let* ((pathname (uiop:ensure-directory-pathname path))) 
    (handler-case
        (progn
          ;; Убедиться, что это каталог (создать при необходимости)
          (ensure-directories-exist pathname)
          ;; Проверить, что truename действительно каталог
          (let* ((truename (truename pathname)))
            (unless (uiop:directory-exists-p truename)
              (error "Путь ~A существует, но не является каталогом." truename))
            (let ((desc (or description
                            (car (last (pathname-directory truename))))))
              (make-instance '<workspace>
                             :path truename
                             :description desc))))
      (file-error (e)
        (error "Не удалось создать или получить доступ к каталогу ~A: ~A"
               path e)))))

(defmethod git-initialized-p ((ws <workspace>))
  "Проверить, инициализирован ли git в рабочем пространстве.
   Использует git-run с командой rev-parse --show-toplevel, чтобы учесть родительские каталоги."
  (let ((path (<workspace>-path ws)))
    (multiple-value-bind (stdout stderr code)
        (cl-git-tree/git-utils:git-run path "rev-parse" "--show-toplevel")
      (declare (ignore stderr))
      (and (zerop code)
           (stringp stdout)
           (> (length (string-trim '(#\Space #\Newline) stdout)) 0)))))

(defmethod git-root ((ws <workspace>))
  "Вернуть корень git-репозитория для рабочего пространства.
   Использует git-run с командой rev-parse --show-toplevel.
   Возвращает NIL если git не инициализирован."
  (when (git-initialized-p ws)
    (let ((g-path 
            (cl-git-tree/shell-utils:shell-run-single
             "." "git" "-C" (namestring (<workspace>-path ws)) "rev-parse" "--show-toplevel")))
      (let ((path-str (string-trim '(#\Space #\Newline) g-path)))
        (when (> (length path-str) 0)
          (uiop/pathname:ensure-directory-pathname 
           (cond
             ((string= "Msys" (cl-git-tree/shell-utils:shell-run-single "." "uname" "-o"))
              (cl-git-tree/shell-utils:shell-run-single "." "cygpath" "-m" path-str))
             (t path-str))))))))

(defmethod git-init ((ws <workspace>)
                     &key bare initial-branch separate-git-dir quiet template shared
                     &allow-other-keys)
  "Инициализировать git-репозиторий с дополнительными опциями."
  (let ((path (<workspace>-path ws)))
    (handler-case
        (progn
          (unless (uiop:directory-exists-p path)
            (uiop:ensure-all-directories-exist path))
          (if (git-initialized-p ws)
              (format t "⚠️ Git-репозиторий уже существует~%")
              (let ((args (list "init")))
                (when bare (push "--bare" args))
                (when quiet (push "--quiet" args))
                (when initial-branch
                  (setf args (append args (list "--initial-branch" initial-branch))))
                (when separate-git-dir
                  (setf args (append args (list "--separate-git-dir" separate-git-dir))))
                (when template
                  (setf args (append args (list "--template" template))))
                (when shared
                  (setf args (append args (list "--shared" shared))))
                (setf args (nreverse args))
                (multiple-value-bind (stdout stderr code)
                    (apply #'cl-git-tree/git-utils:git-run path args)
                  (declare (ignore stdout stderr))
                  (cond
                    ((zerop code)
                     (format t "✅ Git-репозиторий инициализирован: ~A~%" path))
                    (t
                     (format t "❌ Ошибка при инициализации git в ~A (код ~A)~%" path code))))))
          ws)
      (error (c)
        (format t "❌ Не удалось создать каталог ~A: ~A~%" path c)
        ws))))
