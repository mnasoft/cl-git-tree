(in-package :cl-git-tree/loc)

(defmethod repo-pull ((ws <workspace>) (provider <provider>)
                      &key (remote (<location>-id provider)) branch rebase &allow-other-keys)
  "Выполнить git pull из указанного remote.
Возвращает два значения: ws и булев успех (T если код возврата 0)."
  (let* ((root (git-root ws))
         (args (list "pull")))
    ;; Добавляем опции только если они заданы
    (when rebase   (push "--rebase" args))
    ;; Примечание: --ff-only может быть недоступен в старых версиях git.
    ;; Для простоты пока не добавляем, чтобы обеспечить совместимость.
    ;; (when ff-only  (push "--ff-only" args))
    (setf args (append args (list remote)))
    (when branch   (setf args (append args (list branch))))
    
    (multiple-value-bind (stdout stderr code)
        (apply #'cl-git-tree/git-utils:git-run root args)
      (cond
        ((zerop code)
         (format t "~A [~A] Pull ~A~A успешно~%"
                 (find-emo ws "success")
                 remote (repo-name ws) (if branch (format nil "/~A" branch) "")))
        (t
         (format t "~A [~A] Ошибка pull ~A: ~A~%"
                 (find-emo ws "error")
                 remote (repo-name ws)
                 (or stderr stdout "неизвестная ошибка"))))
      (values ws (zerop code)))))
