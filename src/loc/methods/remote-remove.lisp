;;;; ./src/loc/methods/remote-remove.lisp

(in-package :cl-git-tree/loc)

(defmethod remote-remove ((ws <workspace>) (provider <provider>)
                          &key &allow-other-keys)
  "Удалить отдаленный репозиторий для рабочего пространства WORKSPACE,
связанный с провайдером PROVIDER."
  (multiple-value-bind (stdout stderr code)
      (apply #'cl-git-tree/git-utils:git-run
             (git-root ws)
             (list "remote" "remove" (<location>-id provider)))
    (cond
      ((zerop code)
       (format t "🔌 [~A] Remote-репозиторий отключен : ~25A ~A~%" 
               (<location>-id provider)
               (repo-name ws)
               (remote-url ws provider)))
        
      (t
       (format t "❌ Ошибка при отключении remote '~A' из ~A: ~A"
               (<location>-id provider)
               (repo-name ws)
               (or stderr stdout)))))
  ws)
