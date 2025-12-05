;;;; ./src/loc/methods/remote-add.lisp

(in-package :cl-git-tree/loc)

(defmethod remote-add ((ws <workspace>) (provider <provider>) &key &allow-other-keys)
  "Добавить отдаленный репозиторий для рабочего пространства WORKSPACE,
связанный с провайдером PROVIDER."
  (let* ((root (git-root ws))
         (loc-key (<location>-id provider))
         (remote-url (remote-url ws provider)))
    (multiple-value-bind (stdout stderr code)
        (cl-git-tree/git-utils:git-run root "remote")
      (declare (ignore stderr code))
      (if (search loc-key stdout :test #'string=)
          (format t "⚠️  Репозиторий ~A: remote '~A' уже существует~%" (repo-name ws) loc-key)
          (multiple-value-bind (out err code)
              (cl-git-tree/git-utils:git-run root "remote" "add" loc-key remote-url)
            (cond
              ((zerop code)
               (format t "✅ Репозиторий ~A: remote '~A' добавлен → ~A~%"
                       (repo-name ws) loc-key remote-url))
              (t
               (format t "❌ Ошибка при добавлении remote '~A' в ~A: ~A~%"
                       loc-key (repo-name ws) (or err out)))))))
    ws))
