;;;; ./src/loc/methods/remote-add.lisp

(in-package :cl-git-tree/loc)

(defmethod remote-url ((ws <workspace>) (provider <provider>) &key &allow-other-keys)
  (let* ((repo-name (repo-name ws))
         (base-url (cl-git-tree/loc:<location>-url-git provider)))
    (format nil "~A~A.git" base-url repo-name)))

(defmethod remote-add ((ws <workspace>) (provider <provider>) &key &allow-other-keys)
  "Добавить отдаленный репозиторий для рабочего пространства WORKSPACE,
связанный с провайдером PROVIDER."
  (let* ((repo-dir (<workspace>-path ws))
         (loc-key (<location>-id provider))
         (remote-url (remote-url ws provider)))
    (if (member
         loc-key
         (cl-git-tree/shell-utils:shell-run repo-dir "git" "remote")
         :test #'string=)
        (format t "⚠️  В ~A remote ~A уже существует~%" repo-dir loc-key)
        (progn
          (format t "→ ~A: git remote add ~A ~A~%" repo-dir loc-key remote-url)
          (cl-git-tree/shell-utils:shell-run repo-dir "git" "remote" "add" loc-key remote-url)))))
