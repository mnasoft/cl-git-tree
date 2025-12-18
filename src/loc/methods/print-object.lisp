(in-package :cl-git-tree/loc)

(defmethod print-object ((ws <workspace>) stream)
  "Красивый вывод объекта <workspace> в REPL.
   Показывает path, description, git-root и repo-name."
  (print-unreadable-object (ws stream :type t :identity t)
    (format stream "path: ~S~%git-root: ~S~%repo-name: ~S~%desc: ~S"
            (<workspace>-path ws)
            (git-root ws)
            (repo-name ws)
            (<workspace>-description ws))))

(defmethod print-object ((loc <location>) stream)
  (print-unreadable-object (loc stream :type t :identity t)
    (format stream "~%~10T:id      ~20T ~S" (<location>-id loc))
    (format stream "~%~10T:provider~20T ~S" (<location>-provider loc))
    (format stream "~%~10T:url-git ~20T ~S" (<location>-url-git loc))
    (format stream "~%~10T:url-xz  ~20T ~S" (<location>-url-xz loc))
    (format stream "~%~10T:tar     ~20T ~S" (<location>-tar loc))
    (format stream "~%~10T:desc    ~20T ~S" (<location>-description loc))))
