;;;; ./src/loc/methods/repo-clone.lisp

(in-package :cl-git-tree/loc)

(defmethod repo-clone ((ws <workspace>) (provider <provider>) &key &allow-other-keys)
  "Клонировать локальный репозиторий как bare в TARGET-PATH."
  (let* ((repo-dir (<workspace>-path ws))
         (repo-name (cl-git-tree/loc:repo-name  ws))
         (target (cl-git-tree/shell-utils:shell-run-single
                  "." "cygpath" "-m" (remote-url ws provider))))
    (cond
    ;; если уже существует — пропускаем
    ((probe-file target)
     (format t "⚠ ~A: уже существует ~A~%" repo-name target))
    (t
     (ensure-directories-exist target)
     (multiple-value-bind (out err code)
         (cl-git-tree/git-utils:git-run ;; Это неприменимо для linux
          repo-dir
          "clone" "--bare" "." (namestring target))
       (declare (ignore out))
       (if (zerop code)
           (format t "✔ ~A → ~A~%" repo-name target)
           (format t "❌ ~A: clone failed~%~A~%" repo-name err)))))))
