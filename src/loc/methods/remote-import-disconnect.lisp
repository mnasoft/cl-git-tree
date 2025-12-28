(in-package :cl-git-tree/loc)

(defmethod remote-import-disconnect ((ws <workspace>) (provider <provider>) &key (remote-name nil) verbose &allow-other-keys)
  "Отключить временный remote, ранее подключенный через remote-import-connect.
Если remote-name = NIL, берётся шаблон <location>-import.
Возвращает T при успешном отключении или если remote не существует." 
  (let* ((root (git-root ws))
         (loc-id (<location>-id provider))
         (remote (or remote-name (format nil "~A-import" loc-id))))
    (multiple-value-bind (out err code)
        (cl-git-tree/git-utils:git-run root "remote" "remove" remote)
      (cond
        ((zerop code)
         (when verbose (format t "🗑️ Временный remote '~A' удалён~%" remote))
         t)
        (t
         ;; Если команда вернула ошибку, возможно remote не существует — не считаем это фатальным
         (when verbose (format t "⚠️  Не удалось удалить temporary remote '~A': ~A~%" remote (or err out)))
         nil)))))
