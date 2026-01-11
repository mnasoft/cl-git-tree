(in-package :cl-git-tree/loc)

(defmethod repo-status ((ws <workspace>) &key (verbose t) &allow-other-keys)
  "Выводит статус git‑репозитория в рабочем пространстве.
   Если :verbose T (по умолчанию), выводит информационные сообщения.
   Если :verbose NIL, возвращает только строку со статусом."
  (let ((repo-dir (<workspace>-path ws)))
    (multiple-value-bind (out err code)
        (cl-git-tree/git-utils:git-run repo-dir "status" "--short")
      (cond
        ((zerop code)
         (when verbose
           (format t "~%~A ~A~%" (find-emo ws "fs folder") repo-dir)
           (if (string= out "")
               (format t "~A Чисто~%" (find-emo ws "success"))
               (format t "~A~%" out)))
         out)
        (t
         (when verbose
           (format t "~A ~A: git status завершился с кодом ~A:~%~A~%"
                   (find-emo ws "error")
                   repo-dir code err))
         (format nil "Ошибка git status (код ~A)" code))))))
