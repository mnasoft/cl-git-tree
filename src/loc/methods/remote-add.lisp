;;;; ./src/loc/methods/remote-add.lisp

(in-package :cl-git-tree/loc)

(defmethod remote-add ((ws <workspace>) (provider <provider>)
                       &key &allow-other-keys)
  "Добавить отдаленный репозиторий для рабочего пространства WORKSPACE,
связанный с провайдером PROVIDER."
  (multiple-value-bind (stdout stderr code)
      (cl-git-tree/git-utils:git-run (git-root ws) "remote")
    (declare (ignore stderr code))
    (if (search (<location>-id provider) stdout :test #'string=)
        (format t "~A Репозиторий ~A: remote '~A' уже существует~%"
                (find-emo ws "warning")
                (repo-name ws) (<location>-id provider))
        (multiple-value-bind (out err code)
            (cl-git-tree/git-utils:git-run
             (git-root ws) "remote" "add"
             (<location>-id provider)
             (remote-url ws provider))
          (cond
            ((zerop code)
             (format t "~A [~A] Bare-репозиторий подключен: ~25A ~A~%"
                     (find-emo ws "git remote add")
                     (<location>-id provider)
                     (repo-name ws)
                     (remote-url ws provider)))
            (t
             (format t "~A Ошибка при добавлении remote '~A' в ~A: ~A~%"
                     (find-emo ws "error")
                     (<location>-id provider)
                     (repo-name ws)
                     (or err out)))))))
  ws)
