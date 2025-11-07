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
              (make-instance 'cl-git-tree/loc::<workspace>
                             :path truename
                             :description desc))))
      (file-error (e)
        (error "Не удалось создать или получить доступ к каталогу ~A: ~A"
               path e)))))

(defmethod git-initialized-p ((ws cl-git-tree/loc::<workspace>))
  "Проверить, инициализирован ли git в рабочем пространстве.
   Использует git-run с командой rev-parse --show-toplevel, чтобы учесть родительские каталоги."
  (let ((path (cl-git-tree/loc::<workspace>-path ws)))
    (multiple-value-bind (stdout stderr code)
        (cl-git-tree/git-utils:git-run path "rev-parse" "--show-toplevel")
      (declare (ignore stderr))
      (and (zerop code)
           (stringp stdout)
           (> (length (string-trim '(#\Space #\Newline) stdout)) 0)))))

(defmethod git-root ((ws cl-git-tree/loc::<workspace>))
  "Вернуть корень git-репозитория для рабочего пространства.
   Использует git-run с командой rev-parse --show-toplevel."
  (let ((path (cl-git-tree/loc::<workspace>-path ws)))
    (multiple-value-bind (stdout stderr code)
        (cl-git-tree/git-utils:git-run path "rev-parse" "--show-toplevel")
      (declare (ignore stderr))
      (if (zerop code)
          (uiop:ensure-directory-pathname
           (string-trim '(#\Space #\Newline) stdout))
          nil))))

#+nil
(defmethod git-init ((ws <workspace>))
  (let* ((path (<workspace>-path ws))
         (git-dir (merge-pathnames #P".git/" path)))
    (handler-case
        (progn
          (ensure-directories-exist path)
          (if (probe-file git-dir)
              (format t "⚠️ Git-репозиторий уже инициализирован: ~A~%" path)
              (multiple-value-bind (stdout stderr code)
                  (cl-git-tree/git-utils:git-run path "init")
                (declare (ignore stdout stderr))
                (if (zerop code)
                    (format t "✅ Git-репозиторий инициализирован: ~A~%" path)
                    (format t "❌ Ошибка при инициализации git в ~A (код ~A)~%"
                            path code))))
          ws)
      (error (c)
        (format t "❌ Не удалось создать каталог ~A: ~A~%" path c)
        ws))))

(defmethod git-init ((ws cl-git-tree/loc::<workspace>))
  "Инициализировать git-репозиторий в рабочем пространстве.
   Если репозиторий уже существует (в текущем или родительском каталоге),
   ничего не делает и сообщает пользователю."
  (let ((path (cl-git-tree/loc::<workspace>-path ws)))
    (handler-case
        (progn
          ;; Проверяем, существует ли каталог
          (unless (uiop:directory-exists-p path)
            (uiop:ensure-all-directories-exist path))
          ;; Проверяем состояние git через git-root
          (if (git-root ws)
              (format t "⚠️ Git-репозиторий уже существует (корень: ~A)~%"
                      (git-root ws))
              ;; Инициализация нового репозитория
              (multiple-value-bind (stdout stderr code)
                  (cl-git-tree/git-utils:git-run path "init")
                (declare (ignore stdout stderr))
                (cond
                  ((zerop code)
                   (format t "✅ Git-репозиторий инициализирован: ~A~%" path))
                  (t
                   (format t "❌ Ошибка при инициализации git в ~A (код ~A)~%"
                           path code)))))
          ws)
      (error (c)
        (format t "❌ Не удалось создать каталог ~A: ~A~%" path c)
        ws))))
